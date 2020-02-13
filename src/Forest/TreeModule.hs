{-# LANGUAGE MultiParamTypeClasses #-}

module Forest.TreeModule
  ( TreeModule(..)
  , ModuleConstructor
  ) where

import qualified Data.Text   as T

import           Forest.Node

class TreeModule a r where
  edit :: a r -> Path -> T.Text -> IO (Maybe r)
  edit _ _ _ = pure Nothing

  delete :: a r -> Path -> IO (Maybe r)
  delete _ _ = pure Nothing

  reply :: a r -> Path -> T.Text -> IO (Maybe r)
  reply _ _ _ = pure Nothing

  act :: a r -> Path -> IO (Maybe r)
  act _ _ = pure Nothing

type ModuleConstructor a = (Node -> IO ()) -> (a -> IO ()) -> IO ()
