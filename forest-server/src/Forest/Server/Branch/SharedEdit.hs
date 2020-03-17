module Forest.Server.Branch.SharedEdit
  ( SharedEditGlobal
  , sharedEditGlobal
  , SharedEditLocal
  , sharedEditLocal
  , sharedEditDraw
  , sharedEditUpdate
  , sharedEditHandleEvent
  , sharedEditBranch
  ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text             as T

import           Forest.Node
import           Forest.Server.Schema
import           Forest.Server.TreeApp

data SharedEditGlobal = SharedEditGlobal
  { seOnUpdate :: IO ()
  , seNodeVar  :: MVar Node
  }

sharedEditGlobal :: IO () -> T.Text -> IO SharedEditGlobal
sharedEditGlobal onUpdate initialText = do
  nodeVar <- newMVar $ txtNode "r" initialText
  pure SharedEditGlobal
    { seOnUpdate = onUpdate
    , seNodeVar  = nodeVar
    }

data SharedEditLocal = SharedEditLocal
  { seGlobal :: SharedEditGlobal
  , seNode   :: Node
  }

sharedEditLocal :: SharedEditGlobal -> IO SharedEditLocal
sharedEditLocal seg = do
  node <- readMVar $ seNodeVar seg
  pure SharedEditLocal
    { seGlobal = seg
    , seNode   = node
    }

sharedEditDraw :: SharedEditLocal -> Node
sharedEditDraw = seNode

sharedEditUpdate :: SharedEditLocal -> IO SharedEditLocal
sharedEditUpdate sel = do
  node <- readMVar $ seNodeVar $ seGlobal sel
  pure sel{seNode = node}

updateNode :: SharedEditLocal -> (Node -> Node) -> IO SharedEditLocal
updateNode sel f = do
  let seg = seGlobal sel
      nodeVar = seNodeVar seg
  node <- takeMVar nodeVar
  let node' = f node
  putMVar nodeVar node'
  when (node /= node') $ seOnUpdate seg
  pure sel{seNode = node'}

sharedEditHandleEvent :: SharedEditLocal -> Path -> Event e -> IO SharedEditLocal
-- Ignore edits to the top node since it's only reply-able, not edit-able
sharedEditHandleEvent sel (Path []) (Edit _ _) = pure sel
sharedEditHandleEvent sel (Path []) (Delete _) = pure sel
sharedEditHandleEvent sel path (Edit _ text) =
  updateNode sel $ adjustAt (\n -> n {nodeText = text}) path
sharedEditHandleEvent sel path (Delete _) =
  updateNode sel $ deleteAt path
sharedEditHandleEvent sel path (Reply _ text) =
  updateNode sel $ appendAt (txtNode "edr" text) path
sharedEditHandleEvent sel _ _ = pure sel

sharedEditBranch :: SharedEditLocal -> Branch SharedEditLocal e
sharedEditBranch sel = Branch
  { branchNode        = sharedEditDraw sel
  , branchHandleEvent = sharedEditHandleEvent sel
  }
