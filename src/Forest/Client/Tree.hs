module Forest.Client.Tree
  ( Tree
  , newTree
  , switchNode
  , renderTree
  -- * Focused element
  , getCurrent
  , moveUp
  , moveDown
  -- * Folding
  , isCurrentFolded
  , foldCurrent
  , unfoldCurrent
  , toggleFold
  ) where

import           Brick
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Set                   as Set

import           Forest.Client.Node
import           Forest.Client.NodeEditor
import           Forest.Client.ResourceName
import           Forest.Client.WidgetTree
import           Forest.Node
import           Forest.Util

data Tree = Tree
  { treeNode     :: Node
  -- Invariant: The node pointed to by the focused path must always exist
  -- Invariant: The node pointed to by the focused path must not be folded away
  , treeFocused  :: Path
  -- Invariant: The nodes pointed to by the unfolded paths must always exist
  , treeUnfolded :: Set.Set Path
  } deriving (Show)

-- | Find the focus path closest to the input path that still corresponds to a
-- node in the input tree.
findNearestFocus :: Node -> Path -> Path
findNearestFocus _ (Path []) = Path []
findNearestFocus node (Path (x:xs)) = case applyId x node of
  Nothing    -> Path []
  Just child ->
    let (Path childPath) = findNearestFocus child $ Path xs
    in  Path (x:childPath)

-- | Create a new tree, ensuring that all required invariants hold.
newTree :: Node -> Path -> Set.Set Path -> Tree
newTree node focused unfolded = Tree
  { treeNode     = node
  , treeFocused  = safeFocused
  , treeUnfolded = safeUnfolded
  }
  where
    isValidFold :: Node -> Path -> Bool
    isValidFold n p = case applyPath p n of
      Nothing        -> False
      Just childNode -> hasChildren childNode

    foldedNode = applyFolds unfolded node
    safeUnfolded = Set.filter (isValidFold foldedNode) unfolded
    safeFocused = findNearestFocus (applyFolds safeUnfolded node) focused

-- | Switch out a tree's node, keeping as much of the focus and folding
-- information as the type's invariants allow.
switchNode :: Node -> Tree -> Tree
switchNode node tree = newTree node (treeFocused tree) (treeUnfolded tree)

-- | Render a 'Tree' into a widget.
renderTree :: IndentOptions -> Maybe NodeEditor -> Tree -> Widget ResourceName
renderTree opts maybeEditor tree =
  renderWidgetTree opts $ nodeToTree drawState $ treeNode tree
  where
    drawState = DrawState
      { dsEditor = maybeEditor
      , dsFocused = Just $ treeFocused tree
      , dsUnfolded = treeUnfolded tree
      }

{- Focused element -}

-- | Get the currently focused node.
getCurrent :: Tree -> Node
-- We rely on the invariant that the focused node always exists
getCurrent tree = fromJust $ applyPath (treeFocused tree) (treeNode tree)

flatten :: Node -> [Path]
flatten node =
  let flattenedChildren =
        mapChildren (\nid n -> map (Path [nid] <>) $ flatten n) node
   in Path [] : concat flattenedChildren

moveWith :: ((Path -> Bool) -> [Path] -> Maybe Path) -> Tree -> Tree
moveWith finder Tree{treeNode=n, treeFocused=f, treeUnfolded=u} =
  let flattened = flatten $ applyFolds u n
      target = fromMaybe f $ finder (==f) flattened
  in  newTree n target u

-- | Move the focus upward by one node, if possible. Otherwise, do nothing.
moveUp :: Tree -> Tree
moveUp = moveWith findPrev

-- | Move the focus downward by one node, if possible. Otherwise, do nothing.
moveDown :: Tree -> Tree
moveDown = moveWith findNext

{- Folding -}

-- | Check if the currently focused node is folded.
isCurrentFolded :: Tree -> Bool
isCurrentFolded tree = not $ treeFocused tree `Set.member` treeUnfolded tree

-- | Fold the currently focused node. Does nothing if it is already folded.
foldCurrent :: Tree -> Tree
foldCurrent Tree{treeNode=n, treeFocused=f, treeUnfolded=u} =
  newTree n f $ Set.delete f u

-- | Unfold the currently focused node. Does nothing if it is already unfolded.
unfoldCurrent :: Tree -> Tree
unfoldCurrent Tree{treeNode=n, treeFocused=f, treeUnfolded=u} =
  newTree n f $ Set.insert f u

-- | Toggle whether the currently focused node is folded.
toggleFold :: Tree -> Tree
toggleFold tree
  | isCurrentFolded tree = unfoldCurrent tree
  | otherwise            = foldCurrent tree

-- | Remove all nodes that would not be visible due to the folding.
applyFolds :: Set.Set Path -> Node -> Node
applyFolds unfolded node
  | localPath `Set.member` unfolded = node {nodeChildren = foldedChildren}
  | otherwise                       = node {nodeChildren = Map.empty}
  where
    foldedChildren = Map.fromList $ mapChildren applyFoldsToChild node
    applyFoldsToChild nid n = (nid, applyFolds (narrowSet nid unfolded) n)
