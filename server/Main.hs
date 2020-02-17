{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.WebSockets        as WS

import           Forest.Node
import           Forest.Server
import           Forest.TreeModule.Animate
import           Forest.TreeModule.Const
import           Forest.TreeModule.Fork

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
    [ ProngConstructor "Test" $ constModule $ newNode "" "" [txtNode "" "Bla"]
    , ProngConstructor "Animation" $ animateModule 200000 $ map (newNode "" "")
      [ [txtNode "" "|>    |", txtNode "" "Ping!"]
      , [txtNode "" "|->   |", txtNode "" "Ping!"]
      , [txtNode "" "| ->  |", txtNode "" "Ping!"]
      , [txtNode "" "|  -> |", txtNode "" "Ping!"]
      , [txtNode "" "|   ->|", txtNode "" "Ping!"]
      , [txtNode "" "|    -|", txtNode "" "Ping!"]
      , [txtNode "" "|     |", txtNode "" "Ping!"]
      , [txtNode "" "|    <|", txtNode "" "Pong!"]
      , [txtNode "" "|   <-|", txtNode "" "Pong!"]
      , [txtNode "" "|  <- |", txtNode "" "Pong!"]
      , [txtNode "" "| <-  |", txtNode "" "Pong!"]
      , [txtNode "" "|<-   |", txtNode "" "Pong!"]
      , [txtNode "" "|-    |", txtNode "" "Pong!"]
      , [txtNode "" "|     |", txtNode "" "Pong!"]
      ]
    , ProngConstructor "About" $ constModule projectDescriptionNode
    ]
