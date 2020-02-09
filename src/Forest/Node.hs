{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Forest.Node
  (
  -- * Node
    NodeId
  , Node(..)
  , emptyNode
  , initialNode
  , applyId
  , applyPath
  -- * Path
  , Path(..)
  , localPath
  , isLocalPath
  , isValidPath
  , narrowPath
  , narrowSet
  -- * Example values
  , exampleNode
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
  , nodeAct      :: !Bool
  , nodeEdit     :: !Bool
  , nodeDelete   :: !Bool
  , nodeReply    :: !Bool
  , nodeChildren :: !(Map.Map NodeId Node)
  } deriving (Show, Generic)

nodeOptions :: Options
nodeOptions = defaultOptions{fieldLabelModifier = map toLower . drop 4}

instance ToJSON Node where
  toJSON = genericToJSON nodeOptions
  toEncoding = genericToEncoding nodeOptions

instance FromJSON Node where
  parseJSON = genericParseJSON nodeOptions

emptyNode :: T.Text -> Bool -> Bool -> Bool -> Bool -> Node
emptyNode text edit delete reply act = Node text edit delete reply act Map.empty

initialNode :: Node
initialNode = emptyNode "Loading..." False False False False

applyId :: NodeId -> Node -> Maybe Node
applyId nodeId node = nodeChildren node Map.!? nodeId

applyPath :: Path -> Node -> Maybe Node
applyPath (Path ids) node = foldM (flip applyId) node ids

{- Path -}

newtype Path = Path
  { pathElements :: [NodeId]
  } deriving (Show, Eq, Ord, ToJSON, FromJSON)

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

{- For testing -}

exampleNode :: Node
exampleNode = Node "Tool box" False False True False $ Map.fromList
  [ ("hammer", Node "Hammer" False True False True Map.empty)
  , ("nail", Node "Nail" False True False False Map.empty)
  ]