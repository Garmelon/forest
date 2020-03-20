{-# LANGUAGE OverloadedStrings #-}

module Forest.Client
  ( ClientState
  , newClientState
  , runClient
  ) where

import           Brick
import           Brick.BChan
import           Brick.Widgets.Edit
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Graphics.Vty                     as Vty
import qualified Network.WebSockets               as WS

import           Forest.Api
import           Forest.Client.UiState
import           Forest.Client.Websocket
import           Forest.Client.Widgets.WidgetTree
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

deleteNode :: ClientState -> ClientM ()
deleteNode cs =
  when (flagDelete $ nodeFlags $ focusedNode s) $
  liftIO $ sendPacket (csConn cs) $ ClientDelete (focusedPath s)
  where
    s = csUiState cs

actUponNode :: ClientState -> ClientM ()
actUponNode cs =
  when (flagAct $ nodeFlags $ focusedNode s) $
  liftIO $ sendPacket (csConn cs) $ ClientAct (focusedPath s)
  where
    s = csUiState cs

onKeyWithoutEditor :: ClientState -> Vty.Event -> ClientM (Next ClientState)
onKeyWithoutEditor cs (Vty.EvKey k _)
  | k `elem` [Vty.KChar 'q', Vty.KEsc] = halt cs
  | k == Vty.KChar '\t' = onUiState cs toggleFoldAtFocus
  | k `elem` [Vty.KChar 'k', Vty.KUp] = onUiState cs moveFocusUp
  | k `elem` [Vty.KChar 'j', Vty.KDown] = onUiState cs moveFocusDown
  | k `elem` [Vty.KChar 'K', Vty.KPageUp] = onUiState cs moveFocusToPrevSibling
  | k `elem` [Vty.KChar 'J', Vty.KPageDown] =
    onUiState cs moveFocusToNextSibling
  | k `elem` [Vty.KChar 'h', Vty.KLeft] = onUiState cs moveFocusToParent
  | k `elem` [Vty.KChar 'g', Vty.KHome] = onUiState cs moveFocusToTop
  | k `elem` [Vty.KChar 'G', Vty.KEnd] = onUiState cs moveFocusToBottom
  | k == Vty.KChar 'e' = onUiState cs editCurrentNode
  | k == Vty.KChar 'r' = onUiState cs (replyToCurrentNode . unfoldAtFocus)
  | k == Vty.KChar 'R' = onUiState cs replyAfterCurrentNode
  | k `elem` [Vty.KChar 'd', Vty.KChar 'x', Vty.KDel, Vty.KBS] =
    deleteNode cs *> continue cs
  | k `elem` [Vty.KChar 'a', Vty.KChar ' ', Vty.KEnter] =
    actUponNode cs *> continue cs
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
  EventNode node        -> onUiState cs $ replaceRootNode node
  EventConnectionClosed -> halt cs
clientHandleEvent cs _ = continue cs

clientAttrMap :: AttrMap
clientAttrMap = attrMap Vty.defAttr
  [ ("expand", Vty.defAttr `Vty.withStyle` Vty.bold `Vty.withForeColor` Vty.yellow)
  , ("focus", Vty.defAttr `Vty.withBackColor` Vty.blue)
  , ("flags", Vty.defAttr `Vty.withForeColor` Vty.brightBlack)
  , (treeLineAttr, Vty.defAttr `Vty.withForeColor` Vty.brightBlack)
  , (editAttr, Vty.defAttr `Vty.withBackColor` Vty.brightBlack)
  ]

clientApp :: App ClientState Event ResourceName
clientApp = App
  { appDraw         = clientDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = clientHandleEvent
  , appStartEvent   = pure
  , appAttrMap      = const clientAttrMap
  }

runClient :: WS.Connection -> BChan Event -> Node -> IO ()
runClient conn chan node = do
  putStrLn "Starting UI"
  let clientState = newClientState conn node
      vtyBuilder = Vty.mkVty Vty.defaultConfig
  initialVty <- vtyBuilder
  void $ customMain initialVty vtyBuilder (Just chan) clientApp clientState
