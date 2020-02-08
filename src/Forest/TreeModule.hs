module Forest.TreeModule
  ( TreeModule(..)
  , ModuleConstructor
  ) where

import qualified Data.Text   as T

import           Forest.Node

class TreeModule a where
  edit   :: a -> Path -> T.Text -> IO ()
  delete :: a -> Path -> IO ()
  reply  :: a -> Path -> T.Text -> IO ()
  act    :: a -> Path -> IO ()

type ModuleConstructor a = (Node -> IO ()) -> (a -> IO ()) -> IO ()
