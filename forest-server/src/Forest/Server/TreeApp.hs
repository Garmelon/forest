{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | This module specifies a structure for forest server applications. It is
-- based on the way Brick models applications.

module Forest.Server.TreeApp
  ( Next
  , continue
  , halt
  , Event(..)
  , TreeApp(..)
  , simpleConstructor
  , runTreeApp
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Function
import qualified Data.Text              as T
import qualified Network.WebSockets     as WS

import           Forest.Api
import           Forest.Node
import           Forest.Util

data Next a = Continue a | Halt

continue :: a -> Next a
continue = Continue

halt :: Next a
halt = Halt

data Event e
  = Edit Path T.Text
  | Delete Path
  | Reply Path T.Text
  | Act Path
  | Custom e

data TreeApp s e = TreeApp
  { appGraft       :: s -> Node
  , appHandleEvent :: s -> Event e -> IO (Next s)
  , appConstructor :: forall a. (s -> Maybe (TChan e) -> IO a) -> IO a
  }

simpleConstructor :: s -> (s -> IO a) -> IO a
simpleConstructor = (&)

{- The websocket app receiving and sending the packets -}

packetToEvent :: ClientPacket -> Maybe (Event e)
packetToEvent (ClientEdit path text)  = Just $ Edit path text
packetToEvent (ClientDelete path)     = Just $ Delete path
packetToEvent (ClientReply path text) = Just $ Reply path text
packetToEvent (ClientAct path)        = Just $ Act path
packetToEvent (ClientHello _)         = Nothing

receiveThread :: WS.Connection -> TChan (Event e) -> IO ()
receiveThread conn chan = forever $ do
  packet <- receivePacket conn
  case packetToEvent packet of
    -- We can wrap a 'forever' around all of this because closeWithErrorMessage
    -- throws a runtime exception once the connection is closed.
    Nothing    -> closeWithErrorMessage conn "Invalid packet: Hello"
    Just event -> atomically $ writeTChan chan event

data RunState s e = RunState
  { rsEventChan       :: TChan (Event e)
  , rsCustomEventChan :: Maybe (TChan e)
  , rsState           :: s
  , rsNode            :: Node
  }

readEvent :: RunState s e -> STM (Event e)
readEvent rs = case rsCustomEventChan rs of
  Nothing  -> readTChan ec
  Just cec -> readTChan ec `orElse` (Custom <$> readTChan cec)
  where
    ec = rsEventChan rs

sendNodeUpdate :: WS.Connection -> Node -> Node -> IO ()
sendNodeUpdate conn nodeOld nodeNew = case diffNodes nodeOld nodeNew of
  Nothing -> putStrLn "Sending no update because the node didn't change"
  Just (path, updatedNode) -> do
    putStrLn $ "Sending partial update at " ++ show path ++ ": " ++ show updatedNode
    sendPacket conn $ ServerUpdate path updatedNode

runUntilHalt :: WS.Connection -> TreeApp s e -> RunState s e -> IO ()
runUntilHalt conn app rs = do
  event <- atomically $ readEvent rs
  next <- appHandleEvent app (rsState rs) event
  case next of
    Halt -> pure ()
    Continue state' -> do
      let node' = appGraft app state'
      sendNodeUpdate conn (rsNode rs) node'
      runUntilHalt conn app rs{rsState = state', rsNode = node'}

runTreeApp :: Int -> TreeApp s e -> WS.ServerApp
runTreeApp pingDelay app pendingConn = do
  conn <- WS.acceptRequest pendingConn
  chan <- atomically newTChan
  WS.withPingThread conn pingDelay (pure ()) $
    appConstructor app $ \initialState customChan -> do
      firstPacket <- receivePacket conn
      case firstPacket of
        ClientHello _ -> do
          let initialNode = appGraft app initialState
              rs = RunState chan customChan initialState initialNode
          sendPacket conn $ ServerHello [] initialNode
          withThread (receiveThread conn chan) $ runUntilHalt conn app rs
        _ -> closeWithErrorMessage conn "Invalid packet: Expected hello"
