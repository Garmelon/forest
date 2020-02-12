{-# LANGUAGE OverloadedStrings #-}

module Forest.TreeModule.AnimateModule
  ( AnimateModule
  , animateModule
  ) where

import Control.Concurrent

import           Forest.TreeModule
import           Forest.Node
import           Forest.Util

data AnimateModule = AnimateModule

instance TreeModule AnimateModule where

animateModule :: Int -> [Node] -> ModuleConstructor AnimateModule
animateModule delay frames sendNode continue =
  withThread (animateThread frames) $ continue AnimateModule
  where
    animateThread [] = sendNode $ txtNode "" "Invalid animation: No frames provided"
    animateThread (x:xs) = do
      sendNode x
      threadDelay delay
      animateThread $ xs ++ [x]
