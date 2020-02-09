{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Forest.Node
  (
  -- * Node
    NodeId
  , Node(..)
  , hasChildren
  , emptyNode
  , initialNode
  , applyId
  , applyPath
  , mapChildren
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

hasChildren :: Node -> Bool
hasChildren = not . Map.null . nodeChildren

emptyNode :: T.Text -> Bool -> Bool -> Bool -> Bool -> Node
emptyNode text edit delete reply act = Node text edit delete reply act Map.empty

initialNode :: Node
initialNode = emptyNode "Loading..." False False False False

applyId :: NodeId -> Node -> Maybe Node
applyId nodeId node = nodeChildren node Map.!? nodeId

applyPath :: Path -> Node -> Maybe Node
applyPath (Path ids) node = foldM (flip applyId) node ids

mapChildren :: (NodeId -> Node -> a) -> Node -> [a]
mapChildren f node = map (uncurry f) $ Map.toAscList $ nodeChildren node

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

{- For testing -}

exampleNode :: Node
exampleNode =
  Node "forest" False False True True (Map.fromList
    [("0", Node "CHANGELOG.md" True True False False (Map.fromList []))
    , ("1", Node "LICENSE" False False False True (Map.fromList []))
    , ("2", Node "README.md" False False False True (Map.fromList []))
    , ("3", Node "Setup.hs" True True False False (Map.fromList []))
    , ("4", Node "client" True False True False (Map.fromList
      [("0", Node "Main.hs" False True True False (Map.fromList []))
      ]))
    , ("5", Node "forest.cabal" True True True False (Map.fromList []))
    , ("6", Node "gen_file_node.py" True False False True (Map.fromList []))
    , ("7", Node "package.yaml" True False True False (Map.fromList []))
    , ("8", Node "server" True True True False (Map.fromList
      [("0", Node "Main.hs" False False True True (Map.fromList []))
      ]))
    , ("9", Node "src" False False False True (Map.fromList
      [("0", Node "Forest" False True True False (Map.fromList
        [("0", Node "Api.hs" True True True False (Map.fromList []))
        , ("1", Node "Broadcast.hs" False False False False (Map.fromList []))
        , ("2", Node "Client" True True True False (Map.fromList
          [("0", Node "Node.hs" True True True True (Map.fromList []))
          , ("1", Node "NodeEditor.hs" True False False True (Map.fromList []))
          , ("2", Node "ResourceName.hs" True False False False (Map.fromList []))
          , ("3", Node "Tree.hs" False True True True (Map.fromList []))
          , ("4", Node "WidgetTree.hs" True False True False (Map.fromList []))
          ]))
        , ("3", Node "Node.hs" True False False False (Map.fromList []))
        , ("4", Node "Server.hs" False False False False (Map.fromList []))
        , ("5", Node "TreeModule" False True False True (Map.fromList
          [("0", Node "ConstModule.hs" True False False False (Map.fromList []))
          ]))
        , ("6", Node "TreeModule.hs" True True False False (Map.fromList []))
        , ("7", Node "Util.hs" False True False True (Map.fromList []))
        ]))
      , ("1", Node "Forest.hs" False True False False (Map.fromList []))
      ]))
    , ("10", Node "stack.yaml" True False False True (Map.fromList []))
    , ("11", Node "stack.yaml.lock" False False False True (Map.fromList []))
    ])
