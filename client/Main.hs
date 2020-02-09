module Main where

import           Brick
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad
import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty
import qualified Network.WebSockets         as WS

import           Forest.Api
import           Forest.Client.NodeEditor
import           Forest.Client.ResourceName
import           Forest.Client.Tree
import           Forest.Client.WidgetTree
import           Forest.Node
import           Forest.Util

{- Listening for server events -}

data Event = EventNode Node | EventConnectionClosed T.Text

wsClientApp :: Chan Event -> WS.ClientApp ()
wsClientApp eventChan conn = handle handleConnectionException $ forever $ do
  maybePacket <- receivePacket conn
  case maybePacket of
    Nothing -> pure ()
    Just packet -> case packet of
      ServerHello _ node  -> writeChan eventChan (EventNode node)
      -- TODO properly insert node into existing tree
      ServerUpdate _ node -> writeChan eventChan (EventNode node)
  where
    handleConnectionException :: WS.ConnectionException -> IO ()
    handleConnectionException e =
      writeChan eventChan $ EventConnectionClosed $ T.pack $ show e

{- Brick client application-}

data ClientState = ClientState
  { csTree   :: Tree
  , csEditor :: Maybe NodeEditor
  }

newClientState :: Node -> ClientState
newClientState node = ClientState
  { csTree = newTree node
  , csEditor = Nothing
  }

clientDraw :: ClientState -> [Widget ResourceName]
clientDraw cs = [renderTree boxDrawingBranching (csEditor cs) (csTree cs)]

clientHandleEvent :: ClientState -> BrickEvent ResourceName () -> EventM ResourceName (Next ClientState)
clientHandleEvent cs (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt cs
clientHandleEvent cs _                                         = continue cs

clientApp :: App ClientState () ResourceName
clientApp = App
  { appDraw = clientDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = clientHandleEvent
  , appStartEvent = pure
  , appAttrMap = const $ attrMap Vty.defAttr []
  }

main :: IO ()
main = void $ defaultMain clientApp $ newClientState exampleNode
