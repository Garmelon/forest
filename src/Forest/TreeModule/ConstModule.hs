{-# LANGUAGE OverloadedStrings #-}

module Forest.TreeModule.ConstModule
  ( constModule
  ) where

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
  sendNode (emptyNode "Loaded ConstModule" False False False False)
  continue ConstModule
