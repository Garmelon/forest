{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Forest.Node
  ( NodeId
  , enumerateIds
  , findUnusedId
  , NodeFlags(..)
  , readFlags
  , Node(..)
  , newNode
  , txtNode
  , hasChildren
  , mapChildren
  , applyId
  , applyPath
  , adjustAt
  , replaceAt
  , deleteAt
  , appendAt
  , diffNodes
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

import qualified Forest.OrderedMap as OMap

type NodeId = T.Text

enumerateIds :: [NodeId]
enumerateIds = map (T.pack . show) [(0::Integer)..]

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

mapChildren :: (NodeId -> Node -> a) -> Node -> [a]
mapChildren f = map (uncurry f) . OMap.toList . nodeChildren

applyId :: NodeId -> Node -> Maybe Node
applyId nid node = nodeChildren node OMap.!? nid

applyPath :: Path -> Node -> Maybe Node
applyPath (Path ids) node = foldM (flip applyId) node ids

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

newtype Path = Path
  { pathElements :: [NodeId]
  } deriving (Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON)

referencedNodeExists :: Node -> Path -> Bool
referencedNodeExists node path = isJust $ applyPath path node

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
