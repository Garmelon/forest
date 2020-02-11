{-# LANGUAGE OverloadedStrings #-}

module Forest.Server
  ( withThread
  , serverApp
  ) where

import           Control.Concurrent.Chan
import           Control.Monad
import qualified Network.WebSockets      as WS

import           Forest.Api
import           Forest.Node
import           Forest.TreeModule
import           Forest.Util

{- Thread that sends updates to the client -}

sendUpdatesThread :: WS.Connection -> Chan Node -> Node -> IO ()
sendUpdatesThread conn nodeChan _ = do
  node' <- readChan nodeChan
  -- TODO Don't send the whole node every time
  putStrLn $ "Sending full node update with " ++ show node'
  sendPacket conn $ ServerUpdate (Path []) node'
  sendUpdatesThread conn nodeChan node'

{- Main server application that receives and processes client packets -}

receivePackets :: TreeModule a => WS.Connection -> a -> IO ()
receivePackets conn treeModule = forever $ do
  packet <- receivePacket conn
  case packet of
    ClientEdit path text -> do
      putStrLn $ "Editing " ++ show path ++ " to " ++ show text
      edit treeModule path text
    ClientDelete path -> do
      putStrLn $ "Deleting " ++ show path
      delete treeModule path
    ClientReply path text -> do
      putStrLn $ "Replying to " ++ show path ++ " with " ++ show text
      reply treeModule path text
    ClientAct path -> do
      putStrLn $ "Acting upon " ++ show path
      act treeModule path
    ClientHello _ -> closeWithErrorMessage conn "Invalid packet: Hello can only be sent once"

serverApp :: TreeModule a => Int -> ModuleConstructor a -> WS.ServerApp
serverApp pingDelay constructor pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  chan <- newChan
  WS.withPingThread conn pingDelay (pure ()) $ do
    firstPacket <- receivePacket conn
    case firstPacket of
      ClientHello _ -> do
        putStrLn $ "Sending hello reply with " ++ show initialNode
        sendPacket conn $ ServerHello [] initialNode
        withThread (sendUpdatesThread conn chan initialNode) $
          constructor (writeChan chan) $ receivePackets conn
      _ -> closeWithErrorMessage conn "Invalid packet: Expected a hello packet"
  where
    initialNode = txtNode "" "Loading ..."
