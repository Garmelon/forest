{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent.STM
import           Lens.Micro
import           Lens.Micro.TH
import qualified Network.WebSockets              as WS
import           Options.Applicative

import           Forest.Node
import           Forest.Server.Branch.SharedEdit
import           Forest.Server.Schema
import           Forest.Server.TreeApp

{- Command line options -}

data ServerOptions = ServerOptions
  { serverPingDelay :: Int
  , serverHost      :: String
  , serverPort      :: Int
  }

parser :: Parser ServerOptions
parser = ServerOptions
  <$> option auto
      (  long "ping-delay"
      <> help "How many seconds to wait between each ping sent to the client"
      <> value 10
      <> showDefault
      <> metavar "SECONDS"
      )
  <*> strOption
      (  short 'h'
      <> long "host"
      <> help "The server's host"
      <> value (WS.serverHost WS.defaultServerOptions)
      <> showDefault
      <> metavar "HOST"
      )
  <*> option auto
      (  short 'p'
      <> long "port"
      <> help "The port to listen for websocket connections on"
      <> value (WS.serverPort WS.defaultServerOptions)
      <> showDefault
      <> metavar "PORT"
      )

serverOptionsParserInfo :: ParserInfo ServerOptions
serverOptionsParserInfo = info (helper <*> parser) fullDesc

wsOptions :: ServerOptions -> WS.ServerOptions
wsOptions o = WS.defaultServerOptions
  { WS.serverHost        = serverHost o
  , WS.serverPort        = serverPort o
  , WS.serverRequirePong = Just $ serverPingDelay o * 2
  }

{- The actual app -}

data AppEvent = UpdateSharedEdit
  deriving (Show, Eq)

newtype AppState = AppState
  { _asSharedEdit :: SharedEditLocal
  }

makeLenses ''AppState

schema :: AppState -> Schema (Branch AppState AppEvent)
schema s = fork' "Forest"
  [ leaf $ schemaLift asSharedEdit sharedEditBranch s
  ]

draw :: AppState -> Node
draw = schemaDraw . schema

handleEvent :: AppState -> Event AppEvent -> IO (Next AppState)
handleEvent s (Custom UpdateSharedEdit) = do
  sel' <- sharedEditUpdate $ s ^. asSharedEdit
  pure $ continue $ s & asSharedEdit .~ sel'
handleEvent s e = case schemaHandleEvent (schema s) e of
  Nothing -> pure $ continue s
  Just s' -> continue <$> s'

constructor
  :: TChan AppEvent
  -> SharedEditGlobal
  -> (AppState -> Maybe (TChan AppEvent) -> IO a)
  -> IO a
constructor broadcastChan seg cont = do
  sel <- sharedEditLocal seg
  receiveChan <- atomically $ dupTChan broadcastChan
  cont (AppState sel) (Just receiveChan)

main :: IO ()
main = do
  opts <- execParser serverOptionsParserInfo

  putStrLn "Preparing server"
  broadcastChan <- atomically newBroadcastTChan
  let onEditChange = atomically $ writeTChan broadcastChan UpdateSharedEdit
  seg <- sharedEditGlobal onEditChange "Sandbox"
  let app = TreeApp
            { appDraw = draw
            , appHandleEvent = handleEvent
            , appConstructor = constructor broadcastChan seg
            }

  putStrLn "Starting server"
  WS.runServerWithOptions (wsOptions opts) $
    runTreeApp (serverPingDelay opts) app
