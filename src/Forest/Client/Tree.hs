{-# LANGUAGE OverloadedStrings #-}

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
  -- * Example values
  , exampleTree
  ) where

import           Brick
import           Data.List
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Set                   as Set

import           Forest.Client.Node
import           Forest.Client.NodeEditor
import           Forest.Client.ResourceName
import           Forest.Client.WidgetTree
import           Forest.Node

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

-- Warning: 'firstSibling' and 'lastSibling' do their job in a similar, but
-- slightly different way. This can be seen in the return types: 'firstSibling'
-- never returns the original node, 'lastSibling' does if it doesn't have any
-- children.

firstSibling :: Node -> Maybe Path
firstSibling node = case mapChildren (,) node of
  ((nid,_):_) -> Just $ Path [nid]
  _           -> Nothing

lastSibling :: Node -> Path
lastSibling node = case reverse $ mapChildren (,) node of
  ((nid,n):_) -> Path [nid] <> lastSibling n
  _           -> Path []

surroundingSiblings :: Node -> NodeId -> Maybe (Maybe (NodeId, Node), Node, Maybe (NodeId, Node))
surroundingSiblings node middle = do
  let children = mapChildren (,) node
      maybeChildren = [Nothing] ++ map Just children ++ [Nothing]
      triples  = zip3 maybeChildren children (drop 2 maybeChildren)
  (a, (_, b), c) <- find (\(_, (nodeId, _), _) -> nodeId == middle) triples
  pure (a, b, c)

-- | Attempt to find the path of the node that is above the input path.
findPrevNode :: Node -> Path -> Maybe Path
findPrevNode _ (Path []) = Nothing
findPrevNode node (Path (x:xs)) = do
  (maybePrev, childNode, _) <- surroundingSiblings node x
  case findPrevNode childNode (Path xs) of
    Just path -> pure $ Path [x] <> path
    Nothing -> case maybePrev of
      Nothing             -> pure mempty
      Just (prevId, prev) -> pure $ Path [prevId] <> lastSibling prev

-- | Attempt to find the path of the node that is below the input path.
findNextNode :: Node -> Path -> Maybe Path
findNextNode node (Path []) = firstSibling node
findNextNode node (Path (x:xs)) = do
  (_, childNode, maybeNext) <- surroundingSiblings node x
  case findNextNode childNode (Path xs) of
    Just path -> pure $ Path [x] <> path
    Nothing -> do
      (nextId, _) <- maybeNext
      pure $ Path [nextId]

-- | Move the focus upward by one node, if possible. Otherwise, do nothing.
moveUp :: Tree -> Tree
moveUp Tree{treeNode=n, treeFocused=f, treeUnfolded=u} =
  newTree n (fromMaybe f prevNode) u
  where
    prevNode = findPrevNode (applyFolds u n) f

-- | Move the focus downward by one node, if possible. Otherwise, do nothing.
moveDown :: Tree -> Tree
moveDown Tree{treeNode=n, treeFocused=f, treeUnfolded=u} =
  newTree n (fromMaybe f nextNode) u
  where
    nextNode = findNextNode (applyFolds u n) f

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

exampleTree :: Tree
exampleTree = newTree exampleNode (Path ["hammer"]) Set.empty
