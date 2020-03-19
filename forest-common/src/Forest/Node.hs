{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Forest.Node
  (
  -- * Nodes
    NodeId
  , enumerateIds
  , findUnusedId
  , NodeFlags(..)
  , readFlags
  , Node(..)
  , newNode
  , txtNode
  , hasChildren
  , diffNodes
  , flatten
  -- ** Traversing the tree
  , applyId
  , applyPath
  , firstChild
  , lastChild
  , firstSibling
  , prevSibling
  , nextSibling
  , lastSibling
  , firstNode
  , prevNode
  , nextNode
  , lastNode
  -- ** Modifying at a path
  , adjustAt
  , replaceAt
  , deleteAt
  , appendAt
  -- * Paths
  , Path(..)
  , referencedNodeExists
  , splitHeadTail
  , splitInitLast
  , parent
  , narrow
  , narrowSet
  ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.Map.Strict   as Map
import           Data.Maybe
import qualified Data.Set          as Set
import qualified Data.Text         as T
import           Safe

import qualified Forest.OrderedMap as OMap
import           Forest.Util

{- Nodes -}

type NodeId = T.Text

-- | An infinite list of 'NodeId's. Does *not* contain every possible 'NodeId'.
enumerateIds :: [NodeId]
enumerateIds = map (T.pack . show) [(0::Integer)..]

-- | Find a 'NodeId' that is not contained in the given set of IDs. Returns the
-- first matching ID from 'enumerateIds'.
findUnusedId :: Set.Set NodeId -> NodeId
findUnusedId usedIds =
  head $ filter (\nid -> not $ nid `Set.member` usedIds) enumerateIds

data NodeFlags = NodeFlags
  { flagEdit   :: !Bool
  , flagDelete :: !Bool
  , flagReply  :: !Bool
  , flagAct    :: !Bool
  } deriving (Show, Eq)

instance Semigroup NodeFlags where
  f1 <> f2 = NodeFlags
    { flagEdit   = flagEdit f1  || flagEdit f2
    , flagDelete = flagEdit f1  || flagEdit f2
    , flagReply  = flagReply f1 || flagReply f2
    , flagAct    = flagAct f1   || flagAct f2
    }

instance Monoid NodeFlags where
  mempty = NodeFlags
    { flagEdit   = False
    , flagDelete = False
    , flagReply  = False
    , flagAct    = False
    }

readFlags :: String -> NodeFlags
readFlags s = NodeFlags
  { flagEdit   = 'e' `elem` s
  , flagDelete = 'd' `elem` s
  , flagReply  = 'r' `elem` s
  , flagAct    = 'a' `elem` s
  }

-- | A node and its children.
data Node = Node
  { nodeText     :: !T.Text
  , nodeFlags    :: !NodeFlags
  , nodeChildren :: !(OMap.OrderedMap NodeId Node)
  } deriving (Show, Eq)

instance ToJSON Node where
  toJSON node = object
    [ "text" .= nodeText node
    , "edit" .= flagEdit flags
    , "delete" .= flagDelete flags
    , "reply" .= flagReply flags
    , "act" .= flagAct flags
    , "children" .= OMap.toMap children
    , "order" .= OMap.keys children
    ]
    where
      flags = nodeFlags node
      children = nodeChildren node

  toEncoding node = pairs
    (  "text" .= nodeText node
    <> "edit" .= flagEdit flags
    <> "delete" .= flagDelete flags
    <> "reply" .= flagReply flags
    <> "act" .= flagAct flags
    <> "children" .= OMap.toMap children
    <> "order" .= OMap.keys children
    )
    where
      flags = nodeFlags node
      children = nodeChildren node

instance FromJSON Node where
  parseJSON v = parseJSON v >>= \o -> do
    text <- o .: "text"
    flags <- NodeFlags
      <$> o .: "edit"
      <*> o .: "delete"
      <*> o .: "reply"
      <*> o .: "act"
    children <- o .: "children"
    order <- o .: "order"
    pure Node
      { nodeText = text
      , nodeFlags = flags
      , nodeChildren = OMap.fromMapWithOrder children order
      }

newNode :: String -> T.Text -> [Node] -> Node
newNode flags text children = Node
  { nodeText = text
  , nodeFlags = readFlags flags
  , nodeChildren = OMap.fromList $ zip enumerateIds children
  }

txtNode :: String -> T.Text -> Node
txtNode flags text = newNode flags text []

hasChildren :: Node -> Bool
hasChildren = not . OMap.null . nodeChildren

diffNodes :: Node -> Node -> Maybe (Path, Node)
diffNodes a b
  | nodesDiffer || childrenChanged = Just (Path [], b)
  | otherwise = case differingChildren of
      []                   -> Nothing
      [(x, Path xs, node)] -> Just (Path (x:xs), node)
      _                    -> Just (Path [], b)
  where
    nodesDiffer = nodeText a /= nodeText b || nodeFlags a /= nodeFlags b
    aChildren = nodeChildren a
    bChildren = nodeChildren b
    childrenChanged = OMap.keys aChildren /= OMap.keys bChildren
    diffedChildren = Map.toList $ Map.intersectionWith diffNodes (OMap.toMap aChildren) (OMap.toMap bChildren)
    differingChildren = [(key, path, node) | (key, Just (path, node)) <- diffedChildren]

-- | Return the 'Path's to a node and its subnodes in the order they would be
-- displayed in.
flatten :: Node -> [Path]
flatten node = Path [] : flattenedChildren
  where
    flattenChild nid n = map (Path [nid] <>) (flatten n)
    flattenedChildren =
      concat $ OMap.elems $ OMap.mapWithKey flattenChild $ nodeChildren node

{- Traversing the tree -}

applyId :: Node -> NodeId -> Maybe Node
applyId node nid = nodeChildren node OMap.!? nid

applyPath :: Node -> Path -> Maybe Node
applyPath node (Path ids) = foldM applyId node ids

getChild :: ([NodeId] -> Maybe NodeId) -> Node -> Path -> Maybe Path
getChild f root path = do
  node <- applyPath root path
  let childIds = OMap.keys $ nodeChildren node
  childId <- f childIds
  pure $ path <> Path [childId]

firstChild :: Node -> Path -> Maybe Path
firstChild = getChild headMay

lastChild :: Node -> Path -> Maybe Path
lastChild = getChild lastMay

getSibling :: (NodeId -> [NodeId] -> Maybe NodeId) -> Node -> Path -> Maybe Path
getSibling f root path = do
  (parentPath, nodeId) <- splitInitLast path
  parentNode <- applyPath root parentPath
  let siblingIds = OMap.keys $ nodeChildren parentNode
  siblingId <- f nodeId siblingIds
  pure $ parentPath <> Path [siblingId]

firstSibling :: Node -> Path -> Maybe Path
firstSibling = getSibling $ const headMay

prevSibling :: Node -> Path -> Maybe Path
prevSibling = getSibling $ findPrev . (==)

nextSibling :: Node -> Path -> Maybe Path
nextSibling = getSibling $ findNext . (==)

lastSibling :: Node -> Path -> Maybe Path
lastSibling = getSibling $ const lastMay

getNode :: (Path -> [Path] -> Maybe Path) -> Node -> Path -> Maybe Path
getNode f root path = f path $ flatten root

firstNode :: Node -> Path -> Maybe Path
firstNode = getNode $ const headMay

prevNode :: Node -> Path -> Maybe Path
prevNode = getNode $ findPrev . (==)

nextNode :: Node -> Path -> Maybe Path
nextNode = getNode $ findNext . (==)

lastNode :: Node -> Path -> Maybe Path
lastNode = getNode $ const lastMay

{- Modifying at a path -}

adjustAt :: (Node -> Node) -> Path -> Node -> Node
adjustAt f (Path []) node = f node
adjustAt f (Path (x:xs)) node =
  node {nodeChildren = OMap.adjust (adjustAt f $ Path xs) x $ nodeChildren node}

replaceAt :: Node -> Path -> Node -> Node
replaceAt node = adjustAt $ const node

-- | Delete a subnode at a specified path. Does nothing if the path is 'mempty'.
deleteAt :: Path -> Node -> Node
deleteAt path node = case splitInitLast path of
  Nothing -> node
  Just (parentPath, nodeId) -> adjustAt
    (\n -> n{nodeChildren = OMap.delete nodeId $ nodeChildren n})
    parentPath
    node

-- | Append a new child node to the node at the specified path. Chooses an
-- unused node id.
appendAt :: Node -> Path -> Node -> Node
appendAt node =
  adjustAt (\n -> n {nodeChildren = appendAtNewId $ nodeChildren n})
  where
    appendAtNewId m =
      let nid = findUnusedId $ OMap.keysSet m
      in  OMap.append nid node m

{- Paths -}

newtype Path = Path
  { pathElements :: [NodeId]
  } deriving (Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON)

referencedNodeExists :: Node -> Path -> Bool
referencedNodeExists node path = isJust $ applyPath node path

splitHeadTail :: Path -> Maybe (NodeId, Path)
splitHeadTail (Path [])     = Nothing
splitHeadTail (Path (x:xs)) = Just (x, Path xs)

splitInitLast :: Path -> Maybe (Path, NodeId)
splitInitLast (Path []) = Nothing
splitInitLast (Path xs) = Just (Path (init xs), last xs)

parent :: Path -> Maybe Path
parent path = fst <$> splitInitLast path

-- | Try to remove a 'NodeId' from the beginning of a 'Path'.
narrow :: NodeId -> Path -> Maybe Path
narrow nid (Path (x:xs))
  | nid == x = Just (Path xs)
narrow _ _ = Nothing

-- | Narrow a whole set of paths, discarding those that could not be narrowed.
narrowSet :: NodeId -> Set.Set Path -> Set.Set Path
narrowSet nid = Set.fromList . mapMaybe (narrow nid) . Set.toList
