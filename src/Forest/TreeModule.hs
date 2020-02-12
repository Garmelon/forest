module Forest.TreeModule
  ( TreeModule(..)
  , ModuleConstructor
  ) where

import qualified Data.Text   as T

import           Forest.Node

class TreeModule a where
  edit :: a -> Path -> T.Text -> IO ()
  edit _ _ _ = pure ()

  delete :: a -> Path -> IO ()
  delete _ _ = pure ()

  reply :: a -> Path -> T.Text -> IO ()
  reply _ _ _ = pure ()

  act :: a -> Path -> IO ()
  act _ _ = pure ()

type ModuleConstructor a = (Node -> IO ()) -> (a -> IO ()) -> IO ()
