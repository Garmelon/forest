{-# LANGUAGE OverloadedStrings #-}

module Forest.Server
  ( withThread
  , serverApp
  ) where

import           Control.Concurrent.Chan
import           Control.Monad
import qualified Network.WebSockets      as WS

import           Forest.Api
import           Forest.Tree
import           Forest.TreeModule
import           Forest.Util

{- Thread that sends updates to the client -}

sendUpdatesThread :: WS.Connection -> Chan Node -> Node -> IO ()
sendUpdatesThread conn nodeChan _ = do
  newNode <- readChan nodeChan
  -- TODO Don't send the whole node every time
  sendPacket conn $ ServerUpdate (Path []) newNode
  sendUpdatesThread conn nodeChan newNode

{- Main server application that receives and processes client packets -}

receivePackets :: TreeModule a => WS.Connection -> a -> IO ()
receivePackets conn treeModule = forever $ do
  maybePacket <- receivePacket conn
  case maybePacket of
    Nothing -> pure ()
    Just packet ->
      case packet of
        ClientEdit path text -> edit treeModule path text
        ClientDelete path -> delete treeModule path
        ClientReply path text -> reply treeModule path text
        ClientAct path -> act treeModule path
        ClientHello _ -> closeWithErrorMessage conn "Invalid packet: Hello can only be sent once"

serverApp :: TreeModule a => Int -> ModuleConstructor a -> WS.ServerApp
serverApp pingDelay constructor pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  chan <- newChan
  WS.withPingThread conn pingDelay (pure ()) $ do
    firstPacket <- receivePacket conn
    case firstPacket of
      Nothing -> pure ()
      Just (ClientHello _) -> do
        sendPacket conn $ ServerHello [] initialNode
        withThread (sendUpdatesThread conn chan initialNode) $
          constructor (writeChan chan) $ receivePackets conn
      Just _ -> closeWithErrorMessage conn "Invalid packet: Expected a hello packet"
