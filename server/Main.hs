{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.WebSockets            as WS

import           Forest.Server
import           Forest.Node
import           Forest.TreeModule.ConstModule
import           Forest.TreeModule.AnimateModule
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
    , ProngConstructor $ animateModule 200000
      [ newNode "" "Animate" [txtNode "" "|>    |", txtNode "" "Ping!"]
      , newNode "" "Animate" [txtNode "" "|->   |", txtNode "" "Ping!"]
      , newNode "" "Animate" [txtNode "" "| ->  |", txtNode "" "Ping!"]
      , newNode "" "Animate" [txtNode "" "|  -> |", txtNode "" "Ping!"]
      , newNode "" "Animate" [txtNode "" "|   ->|", txtNode "" "Ping!"]
      , newNode "" "Animate" [txtNode "" "|    -|", txtNode "" "Ping!"]
      , newNode "" "Animate" [txtNode "" "|     |", txtNode "" "Ping!"]
      , newNode "" "Animate" [txtNode "" "|    <|", txtNode "" "Pong!"]
      , newNode "" "Animate" [txtNode "" "|   <-|", txtNode "" "Pong!"]
      , newNode "" "Animate" [txtNode "" "|  <- |", txtNode "" "Pong!"]
      , newNode "" "Animate" [txtNode "" "| <-  |", txtNode "" "Pong!"]
      , newNode "" "Animate" [txtNode "" "|<-   |", txtNode "" "Pong!"]
      , newNode "" "Animate" [txtNode "" "|-    |", txtNode "" "Pong!"]
      , newNode "" "Animate" [txtNode "" "|     |", txtNode "" "Pong!"]
      ]
    , ProngConstructor $ constModule projectDescriptionNode
    ]
