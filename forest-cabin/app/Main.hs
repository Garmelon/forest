{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.WebSockets    as WS

import           Forest.Node
import           Forest.Server.TreeApp

pingDelay :: Int
pingDelay = 10

pongDelay :: Int
pongDelay = 3 * pingDelay

options :: WS.ServerOptions
options = WS.defaultServerOptions
  { WS.serverRequirePong = Just pongDelay
  }

app :: TreeApp Node ()
app = TreeApp
  { appGraft = id
  , appHandleEvent = \s _ -> pure $ continue s
  , appConstructor = simpleConstructor $ txtNode "" "Hello world"
  }

main :: IO ()
main = do
  putStrLn "Starting server"
  WS.runServerWithOptions options $ runTreeApp pingDelay Nothing app
