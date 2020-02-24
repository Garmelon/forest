{-# LANGUAGE OverloadedStrings #-}

module Forest.Client
  ( ClientState
  , newClientState
  , runClient
  ) where

import           Brick
import           Brick.BChan
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Graphics.Vty             as Vty
import qualified Network.WebSockets       as WS

import           Forest.Api
import           Forest.Client.UiState
import           Forest.Client.Websocket
import           Forest.Client.WidgetTree
import           Forest.Node
import           Forest.Util

data ResourceName = RnViewport | RnEditor
  deriving (Show, Eq, Ord)

data ClientState = ClientState
  { csUiState :: UiState ResourceName
  , csConn    :: WS.Connection
  }

newClientState :: WS.Connection -> Node -> ClientState
newClientState conn node = ClientState
  { csUiState = newUiState RnEditor node
  , csConn    = conn
  }

{- Handling input events -}

type ClientM a = EventM ResourceName a

onUiState ::
     ClientState
  -> (UiState ResourceName -> UiState ResourceName)
  -> ClientM (Next ClientState)
onUiState cs f = continue cs {csUiState = f $ csUiState cs}

onUiState' ::
     ClientState
  -> (UiState ResourceName -> ClientM (UiState ResourceName))
  -> ClientM (Next ClientState)
onUiState' cs f = do
  s' <- f $ csUiState cs
  continue cs {csUiState = s'}

{- ... without active editor -}

onKeyWithoutEditor :: ClientState -> Vty.Event -> ClientM (Next ClientState)
onKeyWithoutEditor cs (Vty.EvKey k _)
  | k `elem` quitKeys   = halt cs
  | k `elem` foldKeys   = onUiState cs toggleFoldAtFocus
  | k `elem` upKeys     = onUiState cs moveFocusUp
  | k `elem` downKeys   = onUiState cs moveFocusDown
  | k `elem` editKeys   = onUiState cs editCurrentNode
  | k `elem` deleteKeys = do
      when (flagDelete $ nodeFlags $ getFocusedNode $ csUiState cs) $
        liftIO $ sendPacket (csConn cs) $ ClientDelete (getFocusedPath $ csUiState cs)
      continue cs
  | k `elem` replyKeys  = onUiState cs (replyToCurrentNode . unfoldAtFocus)
  | k `elem` replyKeys' = onUiState cs replyAfterCurrentNode
  | k `elem` actKeys    = do
      when (flagAct $ nodeFlags $ getFocusedNode $ csUiState cs) $
        liftIO $ sendPacket (csConn cs) $ ClientAct (getFocusedPath $ csUiState cs)
      continue cs
  where
    quitKeys   = [Vty.KChar 'q', Vty.KEsc]
    foldKeys   = [Vty.KChar '\t']
    upKeys     = [Vty.KChar 'k', Vty.KUp]
    downKeys   = [Vty.KChar 'j', Vty.KDown]
    editKeys   = [Vty.KChar 'e']
    deleteKeys = [Vty.KChar 'd']
    replyKeys  = [Vty.KChar 'r']
    replyKeys'  = [Vty.KChar 'R']
    actKeys    = [Vty.KChar 'a', Vty.KChar ' ', Vty.KEnter]
onKeyWithoutEditor cs _ = continue cs

{- ... with active editor -}

editResultToPacket :: EditResult -> ClientPacket
editResultToPacket result
  | erReply result = ClientReply (erPath result) (erText result)
  | otherwise      = ClientEdit  (erPath result) (erText result)

onKeyWithEditor :: ClientState -> Vty.Event -> ClientM (Next ClientState)
-- Finish editing normally
onKeyWithEditor cs (Vty.EvKey Vty.KEnter _) = do
  let (s', maybeResult) = finishEditing $ csUiState cs
  forM_ maybeResult $ liftIO . sendPacket (csConn cs) . editResultToPacket
  continue cs {csUiState = s'}
-- Abort editing with Escape
onKeyWithEditor cs (Vty.EvKey Vty.KEsc _) = onUiState cs abortEditing
-- Insert a newline on C-n
onKeyWithEditor cs (Vty.EvKey (Vty.KChar 'n') m)
  | Vty.MCtrl `elem` m = onUiState' cs $ updateEditor $ Vty.EvKey Vty.KEnter []
-- Forward all other events as usual
onKeyWithEditor cs ev = onUiState' cs $ updateEditor ev

{- And the rest of the Brick application -}

clientDraw :: ClientState -> [Widget ResourceName]
clientDraw cs = [padTopBottom 1 $ padLeftRight 2 vp]
  where
    tree = renderUiState boxDrawingBranching $ csUiState cs
    vp = viewport RnViewport Vertical tree

clientHandleEvent ::
     ClientState -> BrickEvent ResourceName Event -> ClientM (Next ClientState)
clientHandleEvent cs (VtyEvent ev)
  | isEditorActive (csUiState cs) = onKeyWithEditor cs ev
  | otherwise                     = onKeyWithoutEditor cs ev
clientHandleEvent cs (AppEvent ev) = case ev of
  EventNode node -> onUiState cs $ replaceRootNode node
  EventConnectionClosed -> halt cs
clientHandleEvent cs _ = continue cs

clientAttrMap :: AttrMap
clientAttrMap = attrMap Vty.defAttr
  [ ("expand", Vty.defAttr `Vty.withStyle` Vty.bold `Vty.withForeColor` Vty.yellow)
  , ("focus", Vty.defAttr `Vty.withBackColor` Vty.blue)
  , ("flags", Vty.defAttr `Vty.withForeColor` Vty.brightBlack)
  , (treeLineAttr, Vty.defAttr `Vty.withForeColor` Vty.brightBlack)
  ]

clientApp :: App ClientState Event ResourceName
clientApp = App
  { appDraw = clientDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = clientHandleEvent
  , appStartEvent = pure
  , appAttrMap = const clientAttrMap
  }

runClient :: WS.Connection -> BChan Event -> Node -> IO ()
runClient conn chan node = do
  putStrLn "Starting UI"
  let clientState = newClientState conn node
      vtyBuilder = Vty.mkVty Vty.defaultConfig
  initialVty <- vtyBuilder
  void $ customMain initialVty vtyBuilder (Just chan) clientApp clientState
