{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Forest.Server.TreeModule.Animate
  ( AnimateModule
  , animateModule
  ) where

import           Control.Concurrent

import           Forest.Node
import           Forest.Server.TreeModule
import           Forest.Util

data AnimateModule r = AnimateModule

instance TreeModule AnimateModule () where

animateModule :: Int -> [Node] -> ModuleConstructor (AnimateModule ())
animateModule delay frames sendNode continue =
  withThread (animateThread frames) $ continue AnimateModule
  where
    animateThread [] = sendNode $ txtNode "" "Invalid animation: No frames provided"
    animateThread (x:xs) = do
      sendNode x
      threadDelay delay
      animateThread $ xs ++ [x]
