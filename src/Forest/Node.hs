{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Forest.Node
  (
  -- * Node
    NodeId
  , Node(..)
  , newNode
  , txtNode
  , hasChildren
  , mapChildren
  , applyId
  , applyPath
  , replaceAt
  -- * Path
  , Path(..)
  , localPath
  , isLocalPath
  , isValidPath
  , narrowPath
  , narrowSet
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Char
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Text       as T
import           GHC.Generics

{- Node -}

type NodeId = T.Text

data Node = Node
  { nodeText     :: !T.Text
  , nodeEdit     :: !Bool
  , nodeDelete   :: !Bool
  , nodeReply    :: !Bool
  , nodeAct      :: !Bool
  , nodeChildren :: !(Map.Map NodeId Node)
  } deriving (Show, Generic)

nodeOptions :: Options
nodeOptions = defaultOptions{fieldLabelModifier = map toLower . drop 4}

instance ToJSON Node where
  toJSON = genericToJSON nodeOptions
  toEncoding = genericToEncoding nodeOptions

instance FromJSON Node where
  parseJSON = genericParseJSON nodeOptions

newNode :: String -> T.Text -> [Node] -> Node
newNode flags text children =
  let edit   = 'e' `elem` flags
      delete = 'd' `elem` flags
      reply  = 'r' `elem` flags
      act    = 'a' `elem` flags
      digits = length $ show $ length children
      formatId :: Integer -> T.Text
      formatId = T.justifyRight digits '0' . T.pack . show
      pairedChildren = zip (map formatId [0..]) children
  in  Node text edit delete reply act $ Map.fromList pairedChildren

txtNode :: String -> T.Text -> Node
txtNode flags text = newNode flags text []

hasChildren :: Node -> Bool
hasChildren = not . Map.null . nodeChildren

mapChildren :: (NodeId -> Node -> a) -> Node -> [a]
mapChildren f node = map (uncurry f) $ Map.toAscList $ nodeChildren node

applyId :: NodeId -> Node -> Maybe Node
applyId nodeId node = nodeChildren node Map.!? nodeId

applyPath :: Path -> Node -> Maybe Node
applyPath (Path ids) node = foldM (flip applyId) node ids

replaceAt :: Node -> Path -> Node -> Node
replaceAt childNode (Path []) _ = childNode
replaceAt childNode (Path (x:xs)) node =
  let newChildren = Map.adjust (replaceAt childNode $ Path xs) x $ nodeChildren node
  in  node{nodeChildren = newChildren}

{- Path -}

newtype Path = Path
  { pathElements :: [NodeId]
  } deriving (Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON)

localPath :: Path
localPath = Path []

isLocalPath :: Path -> Bool
isLocalPath = (== localPath)

isValidPath :: Node -> Path -> Bool
isValidPath node path = isJust $ applyPath path node

narrowPath :: NodeId -> Path -> Maybe Path
narrowPath x (Path (y:ys))
  | x == y = Just (Path ys)
narrowPath _ _ = Nothing

narrowSet :: NodeId -> Set.Set Path -> Set.Set Path
narrowSet x s = Set.fromList [Path ys | Path (y:ys) <- Set.toList s, x == y]
