{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Brick
import           Control.Monad.IO.Class
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

withCurrent
  :: (ClientState -> Node -> Path -> ClientM (Next ClientState))
  -> ClientState
  -> ClientM (Next ClientState)
withCurrent f cs = f cs (getCurrent tree) (getCurrentPath tree)
  where
    tree = csTree cs

editAction :: ClientState -> ClientM (Next ClientState)
editAction = withCurrent $ \cs node _ -> do
  let editor = editNode $ nodeText node
  continue $ if nodeEdit node then cs{csEditor = Just editor} else cs

deleteAction :: ClientState -> ClientM (Next ClientState)
deleteAction = withCurrent $ \cs node path -> do
  when (nodeDelete node) $
    liftIO $ sendPacket (csConn cs) $ ClientDelete path
  continue cs

replyAction :: ClientState -> ClientM (Next ClientState)
replyAction = withCurrent $ \cs node _ ->
  continue $ if nodeReply node then cs{csEditor = Just replyToNode} else cs

actAction :: ClientState -> ClientM (Next ClientState)
actAction = withCurrent $ \cs node path -> do
  when (nodeAct node) $
    liftIO $ sendPacket (csConn cs) $ ClientAct path
  continue cs

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
    quitKeys   = [Vty.KChar 'q', Vty.KEsc]
    foldKeys   = [Vty.KChar '\t']
    upKeys     = [Vty.KChar 'k', Vty.KUp]
    downKeys   = [Vty.KChar 'j', Vty.KDown]
    editKeys   = [Vty.KChar 'e']
    deleteKeys = [Vty.KChar 'd']
    replyKeys  = [Vty.KChar 'r']
    actKeys    = [Vty.KChar 'a', Vty.KChar ' ', Vty.KEnter]
onKeyWithoutEditor cs _ = continue cs

{- Actions in edit mode -}

updateEditor :: NodeEditor -> ClientState -> Vty.Event -> ClientM (Next ClientState)
updateEditor ed cs ev = do
  newEd <- handleNodeEditorEvent ev ed
  continue cs{csEditor = Just newEd}

finishEditing :: NodeEditor -> ClientState -> ClientM (Next ClientState)
finishEditing ed = withCurrent $ \cs _ path -> do
  let text = T.intercalate "\n" $ getCurrentText ed
  liftIO $ sendPacket (csConn cs) $
    if asReply ed then ClientReply path text else ClientEdit path text
  continue cs{csEditor = Nothing}

onKeyWithEditor :: NodeEditor -> ClientState -> Vty.Event -> ClientM (Next ClientState)
-- Finish editing normally
onKeyWithEditor ed cs (Vty.EvKey Vty.KEnter _) = finishEditing ed cs
-- Abort editing with Escape
onKeyWithEditor _ cs (Vty.EvKey Vty.KEsc _) = continue cs{csEditor = Nothing}
-- Insert a newline on C-n
onKeyWithEditor ed cs (Vty.EvKey (Vty.KChar 'n') m)
  | Vty.MCtrl `elem` m = updateEditor ed cs $ Vty.EvKey Vty.KEnter []
-- Forward all other events as usual
onKeyWithEditor ed cs ev = updateEditor ed cs ev

{- And the rest of the Brick application -}

clientDraw :: ClientState -> [Widget ResourceName]
clientDraw cs = [padTopBottom 1 $ padLeftRight 2 vp]
  where
    tree = renderTree boxDrawingBranching (csEditor cs) (csTree cs)
    vp = viewport RnViewport Vertical tree

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

receiveUpdates :: BChan Event -> Node -> WS.Connection -> IO ()
receiveUpdates eventChan node conn = do
  packet <- receivePacket conn
  case packet of
    ServerUpdate path subnode -> do
      let node' = replaceAt subnode path node
      writeBChan eventChan $ EventNode node'
      receiveUpdates eventChan node' conn -- Aaand close the loop :D
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

sendCloseEvent :: BChan Event -> SomeException -> IO ()
sendCloseEvent eventChan =
  writeBChan eventChan . EventConnectionClosed . T.pack . show

main :: IO ()
main = do
  opts <- execParser clientOptionsParserInfo
  putStrLn "Connecting to server"
  runCorrectClient opts $ \conn -> do
    putStrLn "Performing initialization ritual"
    node <- performInitialContact conn
    chan <- newBChan 100
    let appState = newClientState chan node conn
    putStrLn "Starting WS thread"
    withThread (handle (sendCloseEvent chan) $ receiveUpdates chan node conn) $ do
      putStrLn "Starting UI"
      let vtyBuilder = Vty.mkVty Vty.defaultConfig
      initialVty <- vtyBuilder
      void $ customMain initialVty vtyBuilder (Just chan) clientApp appState
  putStrLn "Connection closed"
