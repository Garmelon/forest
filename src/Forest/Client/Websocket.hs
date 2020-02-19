{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.Websocket
  ( Event(..)
  , runWithEventChan
  ) where

import           Brick.BChan
import           Control.Exception
import qualified Network.WebSockets    as WS
import qualified Wuss                  as WSS

import           Forest.Api
import           Forest.Client.Options
import           Forest.Node
import           Forest.Util

data Event
  = EventNode Node
  | EventConnectionClosed

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

sendCloseEvent :: BChan Event -> SomeException -> IO ()
sendCloseEvent eventChan e = do
  putStrLn $ "Encountered exception: " ++ show e
  writeBChan eventChan EventConnectionClosed

runWithEventChan :: ClientOptions -> (WS.Connection -> BChan Event -> Node -> IO ()) -> IO ()
runWithEventChan opts f = do
  putStrLn "Connecting to server"
  runCorrectClient opts $ \conn -> do
    putStrLn "Performing initialization ritual"
    node <- performInitialContact conn
    chan <- newBChan 100
    putStrLn "Starting WS thread"
    let wsThread = handle (sendCloseEvent chan) $ receiveUpdates chan node conn
    withThread wsThread $ f conn chan node
  putStrLn "Connection closed and UI stopped"
