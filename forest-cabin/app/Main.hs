{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent.STM
import           Lens.Micro
import           Lens.Micro.TH
import qualified Network.WebSockets              as WS

import           Forest.Node
import           Forest.Server.Branch.SharedEdit
import           Forest.Server.Schema
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
  WS.runServerWithOptions options $ runTreeApp pingDelay app
