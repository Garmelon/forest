{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.MVar
import qualified Network.WebSockets              as WS

import           Forest.Broadcast
import           Forest.Node
import           Forest.Server
import           Forest.TreeModule.Const
import           Forest.TreeModule.Fork
import           Forest.TreeModule.SharedEditing

pingDelay :: Int
pingDelay = 10

pongDelay :: Int
pongDelay = 3 * pingDelay

options :: WS.ServerOptions
options = WS.defaultServerOptions
  { WS.serverRequirePong = Just pongDelay
  }

main :: IO ()
main = do
  putStrLn "Preparing shared edit module"
  sharedEditNodeVar <- newMVar $ txtNode "r" ""
  sharedEditBroadcaster  <- newBroadcaster

  putStrLn "Starting server"
  WS.runServerWithOptions options $ serverApp pingDelay $ forkModule "Forest"
    [ ProngConstructor "Test" $ constModule $ newNode "" "" [txtNode "" "Bla"]
    , ProngConstructor "Sandbox" $ sharedEditingModule sharedEditNodeVar sharedEditBroadcaster
    , ProngConstructor "About" $ constModule projectDescriptionNode
    ]
