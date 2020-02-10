{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Brick
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty
import qualified Network.WebSockets         as WS

import           Forest.Api
import           Forest.Client.NodeEditor
import           Forest.Client.ResourceName
import           Forest.Client.Tree
import           Forest.Client.WidgetTree
import           Forest.Node
import           Forest.Util

{- Listening for server events -}

data Event = EventNode Node | EventConnectionClosed T.Text

sendCloseEvent :: Chan Event -> WS.ConnectionException -> IO ()
sendCloseEvent eventChan e = writeChan eventChan $ EventConnectionClosed $ T.pack $ show e

receiveUpdates :: Chan Event -> Node -> WS.Connection -> IO ()
receiveUpdates eventChan node conn = handle (sendCloseEvent eventChan) $ do
  maybePacket <- receivePacket conn
  case maybePacket of
    -- Wait for ws exception since 'receivePacket' should have already closed
    -- the connection.
    Nothing -> waitForCloseException conn
    Just packet -> case packet of
      ServerUpdate path subnode -> do
        let newNode = replaceAt subnode path node
        writeChan eventChan $ EventNode newNode
        receiveUpdates eventChan newNode conn
      _ -> do
        closeWithErrorMessage conn "Invalid packet: Expected update"
        waitForCloseException conn

wsClientApp :: Chan Event -> WS.ClientApp ()
wsClientApp eventChan conn = handle (sendCloseEvent eventChan) $ do
  maybePacket <- receivePacket conn
  case maybePacket of
    -- Wait for ws exception since 'receivePacket' should have already closed
    -- the connection.
    Nothing -> waitForCloseException conn
    Just (ServerHello _ node) -> do
      writeChan eventChan $ EventNode node
      receiveUpdates eventChan node conn
    _ -> do
      closeWithErrorMessage conn "Invalid packet: Expected hello"
      waitForCloseException conn

{- Brick client application-}

data ClientState = ClientState
  { csTree   :: Tree
  , csEditor :: Maybe NodeEditor
  }

newClientState :: Node -> ClientState
newClientState node = ClientState
  { csTree = newTree node localPath Set.empty
  , csEditor = Nothing
  }

type ClientM a = EventM ResourceName a

{- Normal actions -}

quitKeys :: [Vty.Key]
quitKeys = [Vty.KEsc, Vty.KChar 'q']

foldKeys :: [Vty.Key]
foldKeys = [Vty.KChar '\t']

foldAction :: ClientState -> ClientM (Next ClientState)
foldAction cs = continue cs{csTree = toggleFold $ csTree cs}

upKeys :: [Vty.Key]
upKeys = [Vty.KUp, Vty.KChar 'k']

upAction :: ClientState -> ClientM (Next ClientState)
upAction cs = continue cs{csTree = moveUp $ csTree cs}

downKeys :: [Vty.Key]
downKeys = [Vty.KDown, Vty.KChar 'j']

downAction :: ClientState -> ClientM (Next ClientState)
downAction cs = continue cs{csTree = moveDown $ csTree cs}

editKeys :: [Vty.Key]
editKeys = [Vty.KChar 'e']

editAction :: ClientState -> ClientM (Next ClientState)
editAction cs =
  let node = getCurrent $ csTree cs
      editor = editNode $ nodeText node
  in  continue cs{csEditor = Just editor}

deleteKeys :: [Vty.Key]
deleteKeys = [Vty.KChar 'e']

replyKeys :: [Vty.Key]
replyKeys = [Vty.KChar 'r']

replyAction :: ClientState -> ClientM (Next ClientState)
replyAction cs = continue cs{csEditor = Just replyToNode}

actKeys :: [Vty.Key]
actKeys = [Vty.KEnter, Vty.KChar 'a']

onKeyWithoutEditor
  :: ClientState
  -> Vty.Event
  -> EventM ResourceName (Next ClientState)
onKeyWithoutEditor cs (Vty.EvKey k _)
  | k `elem` quitKeys  = halt cs
  | k `elem` foldKeys  = foldAction cs
  | k `elem` upKeys    = upAction cs
  | k `elem` downKeys  = downAction cs
  | k `elem` editKeys  = editAction cs
  | k `elem` replyKeys = replyAction cs
onKeyWithoutEditor cs _ = continue cs

{- Editor actions -}

updateEditor :: NodeEditor -> ClientState -> Vty.Event -> ClientM (Next ClientState)
updateEditor ed cs ev = do
  newEd <- handleNodeEditorEvent ev ed
  continue cs{csEditor = Just newEd}

onKeyWithEditor :: NodeEditor -> ClientState -> Vty.Event -> ClientM (Next ClientState)
-- Abort editing with Escape
onKeyWithEditor _ cs (Vty.EvKey Vty.KEsc _) = continue cs{csEditor = Nothing}
-- Insert a newline on C-n
onKeyWithEditor ed cs (Vty.EvKey (Vty.KChar 'n') m)
  | Vty.MCtrl `elem` m = updateEditor ed cs $ Vty.EvKey Vty.KEnter []
-- Forward all other events as usual
onKeyWithEditor ed cs ev = updateEditor ed cs ev

{- Constructing the client app -}

clientDraw :: ClientState -> [Widget ResourceName]
clientDraw cs = [padTop (Pad 1) $ padLeft (Pad 2) tree]
  where
    tree = renderTree boxDrawingBranching (csEditor cs) (csTree cs)

clientHandleEvent :: ClientState -> BrickEvent ResourceName () -> EventM ResourceName (Next ClientState)
clientHandleEvent cs (VtyEvent ev) = case csEditor cs of
  Nothing -> onKeyWithoutEditor cs ev
  Just ed -> onKeyWithEditor ed cs ev
clientHandleEvent cs _ = continue cs

clientAttrMap :: AttrMap
clientAttrMap = attrMap Vty.defAttr
  [ ("expand", Vty.currentAttr `Vty.withStyle` Vty.bold `Vty.withForeColor` Vty.yellow)
  , ("focus", Vty.currentAttr `Vty.withBackColor` Vty.blue)
  , ("flags", Vty.currentAttr `Vty.withForeColor` Vty.brightBlack)
  ]

clientApp :: App ClientState () ResourceName
clientApp = App
  { appDraw = clientDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = clientHandleEvent
  , appStartEvent = pure
  , appAttrMap = const clientAttrMap
  }

main :: IO ()
main = void $ defaultMain clientApp $ newClientState exampleNode
