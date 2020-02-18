{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Forest.TreeModule.SharedEditing
  ( SharedEditingModule
  , sharedEditingModule
  ) where

import           Control.Concurrent.MVar
import           Control.Monad

import           Forest.Broadcast
import           Forest.Node
import           Forest.TreeModule
import           Forest.Util

data SharedEditingModule r = SharedEditingModule
  { seNodeVar     :: MVar Node
  , seBroadcaster :: Broadcaster Node
  }

instance TreeModule SharedEditingModule r where
  edit _ (Path []) _ = pure Nothing
  edit se path text = do
    node' <- modifyMVar (seNodeVar se) $ \node -> do
      let updatedNode = adjustAt (\n -> n{nodeText = text}) path node
      pure (updatedNode, updatedNode)
    broadcast (seBroadcaster se) node'
    pure Nothing

  delete _ (Path []) = pure Nothing
  delete se path = do
    node' <- modifyMVar (seNodeVar se) $ \node -> do
      let updatedNode = deleteAt path node
      pure (updatedNode, updatedNode)
    broadcast (seBroadcaster se) node'
    pure Nothing

  reply se path text = do
    node' <- modifyMVar (seNodeVar se) $ \node -> do
      let updatedNode = appendAt (txtNode "edr" text) path node
      pure (updatedNode, updatedNode)
    broadcast (seBroadcaster se) node'
    pure Nothing

sharedEditingModule ::
     MVar Node -> Broadcaster Node -> ModuleConstructor (SharedEditingModule ())
sharedEditingModule nodeVar broadcaster sendNode continue = do
  listener <- attachListener broadcaster
  withThread (updateOnNewBroadcast listener) $ do
    withMVar nodeVar sendNode -- We need to show our initial edit state
    continue $ SharedEditingModule nodeVar broadcaster
  where
    updateOnNewBroadcast listener = forever $ do
      node <- listen listener
      sendNode node
