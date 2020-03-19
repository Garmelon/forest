module Forest.Client.NodeUtil
  ( Unfolded
  , foldVisibleNodes
  , applyFolds
  , findPrevNode
  , findNextNode
  ) where

import           Data.Maybe
import qualified Data.Set          as Set

import           Forest.Node
import qualified Forest.OrderedMap as OMap
import           Forest.Util

type Unfolded = Set.Set Path

foldVisibleNodes' :: Path -> (Path -> Node -> Maybe [a] -> a) -> Unfolded -> Node -> a
foldVisibleNodes' path f unfolded node
  | childrenVisible = f path node $ Just mappedChildren
  | otherwise       = f path node Nothing
  where
    childrenVisible = mempty `Set.member` unfolded
    mappedChildren = map (uncurry goDeeper) $ OMap.toList $ nodeChildren node
    goDeeper nid = foldVisibleNodes' (path <> Path [nid]) f (narrowSet nid unfolded)

-- | The word "fold" in the name of this function is meant as in 'foldr'. This
-- function folds a tree of nodes while respecting which nodes should be visible
-- according to the 'Unfolded' set.
foldVisibleNodes :: (Path -> Node -> Maybe [a] -> a) -> Unfolded -> Node -> a
foldVisibleNodes = foldVisibleNodes' mempty

-- | Keep only those nodes that are visible according to the 'Unfolded' set.
applyFolds :: Unfolded -> Node -> Node
applyFolds unfolded node
  | mempty `Set.member` unfolded = node {nodeChildren = children}
  | otherwise = node {nodeChildren = OMap.empty}
  where
    children =
      OMap.mapWithKey (\nid -> applyFolds $ narrowSet nid unfolded) $
      nodeChildren node

findPrevNode :: Node -> Path -> Path
findPrevNode node path = fromMaybe path $ findPrev (==path) $ flatten node

findNextNode :: Node -> Path -> Path
findNextNode node path = fromMaybe path $ findNext (==path) $ flatten node
