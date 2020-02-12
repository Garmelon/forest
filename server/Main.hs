{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.WebSockets            as WS

import           Forest.Server
import           Forest.Node
import           Forest.TreeModule.ConstModule
import           Forest.TreeModule.ForkModule

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
  putStrLn "Starting server"
  WS.runServerWithOptions options $ serverApp pingDelay $ forkModule "Forest"
    [ ProngConstructor $ constModule $ newNode "" "Test" [txtNode "" "Bla"]
    , ProngConstructor $ constModule projectDescriptionNode
    ]
