module Forest.TreeModule
  ( TreeModule(..)
  , ModuleConstructor
  ) where

import qualified Data.Text   as T

import           Forest.Node

class TreeModule a where
  edit :: a -> Path -> T.Text -> IO Bool
  edit _ _ _ = pure True

  delete :: a -> Path -> IO Bool
  delete _ _ = pure True

  reply :: a -> Path -> T.Text -> IO Bool
  reply _ _ _ = pure True

  act :: a -> Path -> IO Bool
  act _ _ = pure True

type ModuleConstructor a = (Node -> IO ()) -> (a -> IO ()) -> IO ()
