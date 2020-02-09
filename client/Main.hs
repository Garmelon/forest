{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad
import qualified Data.Set                   as Set
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
  { csTree = newTree node localPath Set.empty
  , csEditor = Nothing
  }

clientDraw :: ClientState -> [Widget ResourceName]
clientDraw cs = [joinBorders $ withBorderStyle unicode $ debug <=> tree]
  where
    tree = borderWithLabel (txt "Tree") $ renderTree boxDrawingBranching (csEditor cs) (csTree cs)
    debug = borderWithLabel (txt "Debug") $ hLimit 80 $ txtWrap $ T.pack $ show $ csTree cs

isQuitEvent :: BrickEvent a b -> Bool
isQuitEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = True
isQuitEvent _                                         = False

isFocusDownEvent :: BrickEvent a b -> Bool
isFocusDownEvent (VtyEvent (Vty.EvKey (Vty.KChar 'j') [])) = True
isFocusDownEvent _                                         = False

isFocusUpEvent :: BrickEvent a b -> Bool
isFocusUpEvent (VtyEvent (Vty.EvKey (Vty.KChar 'k') [])) = True
isFocusUpEvent _                                         = False

isToggleFoldEvent :: BrickEvent a b -> Bool
isToggleFoldEvent (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = True
isToggleFoldEvent _                                          = False

clientHandleEvent :: ClientState -> BrickEvent ResourceName () -> EventM ResourceName (Next ClientState)
clientHandleEvent cs e
  | isQuitEvent e       = halt cs
  | isFocusUpEvent e    = continue cs{csTree = moveUp $ csTree cs}
  | isFocusDownEvent e  = continue cs{csTree = moveDown $ csTree cs}
  | isToggleFoldEvent e = continue cs{csTree = toggleFold $ csTree cs}
  | otherwise           = continue cs

clientAttrMap :: AttrMap
clientAttrMap = attrMap Vty.defAttr
  [ ("expand", Vty.currentAttr `Vty.withStyle` Vty.bold `Vty.withForeColor` Vty.yellow)
  , ("focus", Vty.currentAttr `Vty.withBackColor` Vty.blue)
  ]

clientApp :: App ClientState () ResourceName
clientApp = App
  { appDraw = clientDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = clientHandleEvent
  , appStartEvent = pure
  , appAttrMap = const clientAttrMap
  }

main :: IO ()
main = void $ defaultMain clientApp testState
  where
    testState = ClientState {csTree = exampleTree, csEditor = Nothing}
