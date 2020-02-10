{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Brick
import           Brick.BChan
import           Control.Exception
import           Control.Monad
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty
import qualified Network.WebSockets         as WS
import           Options.Applicative
import qualified Wuss                       as WSS

import           Forest.Api
import           Forest.Client.NodeEditor
import           Forest.Client.Options
import           Forest.Client.ResourceName
import           Forest.Client.Tree
import           Forest.Client.WidgetTree
import           Forest.Node
import           Forest.Util

{- First, the UI types -}

data Event = EventNode Node
  | EventConnectionClosed T.Text

data ClientState = ClientState
  { csTree      :: Tree
  , csEditor    :: Maybe NodeEditor
  , csConn      :: WS.Connection
  , csEventChan :: BChan Event
  }

newClientState :: BChan Event -> Node -> WS.Connection -> ClientState
newClientState eventChan node conn = ClientState
  { csTree      = newTree node localPath Set.empty
  , csEditor    = Nothing
  , csConn      = conn
  , csEventChan = eventChan
  }

type ClientM a = EventM ResourceName a

{- Actions in normal mode -}

foldAction :: ClientState -> ClientM (Next ClientState)
foldAction cs = continue cs{csTree = toggleFold $ csTree cs}

upAction :: ClientState -> ClientM (Next ClientState)
upAction cs = continue cs{csTree = moveUp $ csTree cs}

downAction :: ClientState -> ClientM (Next ClientState)
downAction cs = continue cs{csTree = moveDown $ csTree cs}

editAction :: ClientState -> ClientM (Next ClientState)
editAction cs =
  let node = getCurrent $ csTree cs
      editor = editNode $ nodeText node
  in  continue cs{csEditor = Just editor}

deleteAction :: ClientState -> ClientM (Next ClientState)
deleteAction cs = continue cs -- TODO implement

replyAction :: ClientState -> ClientM (Next ClientState)
replyAction cs = continue cs{csEditor = Just replyToNode}

actAction :: ClientState -> ClientM (Next ClientState)
actAction cs = continue cs -- TODO implement

onKeyWithoutEditor :: ClientState -> Vty.Event -> EventM ResourceName (Next ClientState)
onKeyWithoutEditor cs (Vty.EvKey k _)
  | k `elem` quitKeys   = halt cs
  | k `elem` foldKeys   = foldAction cs
  | k `elem` upKeys     = upAction cs
  | k `elem` downKeys   = downAction cs
  | k `elem` editKeys   = editAction cs
  | k `elem` deleteKeys = deleteAction cs
  | k `elem` replyKeys  = replyAction cs
  | k `elem` actKeys    = actAction cs
  where
    quitKeys   = [Vty.KEsc, Vty.KChar 'q']
    foldKeys   = [Vty.KChar '\t']
    upKeys     = [Vty.KUp, Vty.KChar 'k']
    downKeys   = [Vty.KDown, Vty.KChar 'j']
    editKeys   = [Vty.KChar 'e']
    deleteKeys = [Vty.KChar 'e']
    replyKeys  = [Vty.KChar 'r']
    actKeys    = [Vty.KEnter, Vty.KChar 'a']
onKeyWithoutEditor cs _ = continue cs

{- Actions in edit mode -}

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

{- And the rest of the Brick application -}

clientDraw :: ClientState -> [Widget ResourceName]
clientDraw cs = [padTop (Pad 1) $ padLeft (Pad 2) tree]
  where
    tree = renderTree boxDrawingBranching (csEditor cs) (csTree cs)

clientHandleEvent
  :: ClientState
  -> BrickEvent ResourceName Event
  -> ClientM (Next ClientState)
clientHandleEvent cs (VtyEvent ev) = case csEditor cs of
  Nothing -> onKeyWithoutEditor cs ev
  Just ed -> onKeyWithEditor ed cs ev
clientHandleEvent cs (AppEvent ev) = case ev of
  EventNode node -> continue cs{csTree = replaceNode node $ csTree cs}
  EventConnectionClosed _ -> halt cs
clientHandleEvent cs _ = continue cs

clientAttrMap :: AttrMap
clientAttrMap = attrMap Vty.defAttr
  [ ("expand", Vty.currentAttr `Vty.withStyle` Vty.bold `Vty.withForeColor` Vty.yellow)
  , ("focus", Vty.currentAttr `Vty.withBackColor` Vty.blue)
  , ("flags", Vty.currentAttr `Vty.withForeColor` Vty.brightBlack)
  ]

clientApp :: App ClientState Event ResourceName
clientApp = App
  { appDraw = clientDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = clientHandleEvent
  , appStartEvent = pure
  , appAttrMap = const clientAttrMap
  }

{- And now for the websocket connection handling -}

performInitialContact :: WS.Connection -> IO Node
performInitialContact conn = do
  -- First, the client must send a hello packet containing the protocol
  -- extensions it requests.
  sendPacket conn $ ClientHello []
  -- Then, the server must reply with a hello packet containing the extensions
  -- that will be active for this connection, and an initial node.
  serverReply <- receivePacket conn
  case serverReply of
    (ServerHello [] node) -> pure node
    -- Since the client never requests any protocol extensions, the server must
    -- also reply with an empty list of extensions.
    (ServerHello _ _) -> closeWithErrorMessage conn "Invalid protocol extensions"
    _ -> closeWithErrorMessage conn "Invalid packet: Expected hello"

sendCloseEvent :: BChan Event -> WS.ConnectionException -> IO ()
sendCloseEvent eventChan =
  writeBChan eventChan . EventConnectionClosed . T.pack . show

receiveUpdates :: BChan Event -> Node -> WS.Connection -> IO ()
receiveUpdates eventChan node conn = handle (sendCloseEvent eventChan) $ do
  packet <- receivePacket conn
  case packet of
    ServerUpdate path subnode -> do
      let newNode = replaceAt subnode path node
      writeBChan eventChan $ EventNode newNode
      receiveUpdates eventChan newNode conn -- Aaand close the loop :D
    _ -> closeWithErrorMessage conn "Invalid packet: Expected update"

runCorrectClient :: ClientOptions -> WS.ClientApp a -> IO a
runCorrectClient opts app
  | ssl = WSS.runSecureClient name (fromInteger $ toInteger port) path app
  | otherwise = WS.runClient name port path app
  where
    -- I found this nicer to read than (ab-)using record syntax in the arguments
    name = clientHostName opts
    port = clientPort opts
    path = clientPath opts
    ssl = clientSsl opts

{- Gluing everything together -}

main :: IO ()
main = do
  opts <- execParser clientOptionsParserInfo
  runCorrectClient opts $ \conn -> do
    node <- performInitialContact conn
    chan <- newBChan 100
    let appState = newClientState chan node conn
    withThread (receiveUpdates chan node conn) $ do
      let vtyBuilder = Vty.mkVty Vty.defaultConfig
      initialVty <- vtyBuilder
      void $ customMain initialVty vtyBuilder (Just chan) clientApp appState
