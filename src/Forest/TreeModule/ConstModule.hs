{-# LANGUAGE OverloadedStrings #-}

module Forest.TreeModule.ConstModule
  ( constModule
  ) where

import           Control.Concurrent
import           Control.Monad

import           Forest.Node
import           Forest.TreeModule

data ConstModule = ConstModule

instance TreeModule ConstModule where
  edit _ _ _ = pure ()
  delete _ _ = pure ()
  reply _ _ _ = pure ()
  act _ _ = pure ()

constModule :: ModuleConstructor ConstModule
constModule sendNode continue = do
  void $ forkIO $ do
    threadDelay $ 1000 * 1000 -- One second
    sendNode (emptyNode "Loaded ConstModule" False False False False)
  continue ConstModule
