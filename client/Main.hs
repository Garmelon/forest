module Main where

import           Options.Applicative

import           Forest.Client
import           Forest.Client.Options
import           Forest.Client.Websocket

main :: IO ()
main = do
  opts <- execParser clientOptionsParserInfo
  runWithEventChan opts runClient
