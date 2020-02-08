module Forest.Client.Tree
  ( Tree(..)
  , newTree
  , renderTree
  , toggleFold
  , moveFocusUp
  , moveFocusDown
  , switchNode
  ) where

import           Brick
import qualified Data.Set                   as Set

import           Forest.Client.Node
import           Forest.Client.NodeEditor
import           Forest.Client.ResourceName
import           Forest.Node

data Tree = Tree
  { treeNode     :: Node
  , treeFocused  :: Path
  , treeUnfolded :: Set.Set Path
  } deriving (Show)

newTree :: Node -> Tree
newTree node = Tree
  { treeNode     = node
  , treeFocused  = Path []
  , treeUnfolded = Set.empty
  }

renderTree :: Maybe NodeEditor -> Tree -> Widget ResourceName
renderTree maybeEditor tree = renderNode drawState $ treeNode tree
  where
    drawState = DrawState
      { dsEditor   = maybeEditor
      , dsFocused  = Just $ treeFocused tree
      , dsUnfolded = treeUnfolded tree
      }

isCurrentFolded :: Tree -> Bool
isCurrentFolded tree = treeFocused tree `Set.member` treeUnfolded tree

foldCurrent :: Tree -> Tree
foldCurrent tree@Tree {treeFocused = f, treeUnfolded = u} =
  tree {treeUnfolded = Set.delete f u}

unfoldCurrent :: Tree -> Tree
unfoldCurrent tree@Tree {treeFocused = f, treeUnfolded = u} =
  tree {treeUnfolded = Set.insert f u}

toggleFold :: Tree -> Tree
toggleFold tree
  | isCurrentFolded tree = unfoldCurrent tree
  | otherwise            = foldCurrent tree

moveFocusUp :: Tree -> Tree
moveFocusUp = id -- TODO implement properly

moveFocusDown :: Tree -> Tree
moveFocusDown = id -- TODO implement properly

findNearestFocus :: Node -> Path -> Path
findNearestFocus _ _ = localPath -- TODO implement properly

switchNode :: Node -> Tree -> Tree
switchNode node tree = Tree
  { treeNode = node
  , treeFocused = findNearestFocus node $ treeFocused tree
  , treeUnfolded = Set.filter (isValidPath node) $ treeUnfolded tree
  }
