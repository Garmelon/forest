{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad
import qualified Network.WebSockets      as WS

import           Forest.Node
import           Forest.Server.TreeApp

{- Websocket server stuff -}

pingDelay :: Int
pingDelay = 10

pongDelay :: Int
pongDelay = 3 * pingDelay

options :: WS.ServerOptions
options = WS.defaultServerOptions
  { WS.serverRequirePong = Just pongDelay
  }

{- The actual app -}

data AppEvent = SharedNodeEdited
  deriving (Show, Eq)

data AppState = AppState
  { asBroadcastChan :: TChan AppEvent
  , asReceiveChan   :: TChan AppEvent
  , asSharedNodeVar :: MVar Node
  , asSharedNode    :: Node
  }

graft :: AppState -> Node
graft = asSharedNode

updateSharedNode :: AppState -> (Node -> Node) -> IO AppState
updateSharedNode s f = do
  node <- takeMVar $ asSharedNodeVar s
  let node' = f node
  putMVar (asSharedNodeVar s) node'
  when (node /= node') $ atomically $ do
    writeTChan (asBroadcastChan s) SharedNodeEdited
    void $ readTChan $ asReceiveChan s
  pure s{asSharedNode = node'}

handleEvent :: AppState -> Event AppEvent -> IO (Next AppState)
handleEvent s (Custom SharedNodeEdited) = do
  node <- readMVar $ asSharedNodeVar s
  pure $ continue s{asSharedNode = node}
handleEvent s (Edit path text) = do
  s' <- updateSharedNode s $ adjustAt (\n -> n{nodeText = text}) path
  pure $ continue s'
handleEvent s (Delete path) = do
  s' <- updateSharedNode s $ deleteAt path
  pure $ continue s'
handleEvent s (Reply path text) = do
  s' <- updateSharedNode s $ appendAt (txtNode "edr" text) path
  pure $ continue s'
handleEvent s _ = do
  pure $ continue s

constructor
  :: TChan AppEvent
  -> MVar Node
  -> (AppState -> Maybe (TChan AppEvent) -> IO a)
  -> IO a
constructor broadcastChan sharedNodeVar cont = do
  node <- readMVar sharedNodeVar
  receiveChan <- atomically $ dupTChan broadcastChan
  let state = AppState broadcastChan receiveChan sharedNodeVar node
  cont state $ Just receiveChan

main :: IO ()
main = do
  putStrLn "Preparing shared editing"
  sharedNodeVar <- newMVar $ txtNode "r" "Sandbox"
  broadcastChan <- atomically newBroadcastTChan
  let app = TreeApp
            { appGraft = graft
            , appHandleEvent = handleEvent
            , appConstructor = constructor broadcastChan sharedNodeVar
            }

  putStrLn "Starting server"
  WS.runServerWithOptions options $ runTreeApp pingDelay app
