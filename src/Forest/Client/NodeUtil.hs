module Forest.Client.NodeUtil
  ( Unfolded
  , foldVisibleNodes
  , applyFolds
  , flatten
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
applyFolds = foldVisibleNodes (\_ node _ -> node)

-- | Return the 'Path's to a node and its subnodes in the order they would be
-- displayed in.
flatten :: Node -> [Path]
flatten node =
  let flattenedChildren =
        mapChildren (\nid n -> map (Path [nid] <>) $ flatten n) node
   in Path [] : concat flattenedChildren

findPrevNode :: Node -> Path -> Path
findPrevNode node path = fromMaybe path $ findPrev (==path) $ flatten node

findNextNode :: Node -> Path -> Path
findNextNode node path = fromMaybe path $ findNext (==path) $ flatten node
