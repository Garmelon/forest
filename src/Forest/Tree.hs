{-# LANGUAGE OverloadedStrings #-}

module Forest.Tree
  (
  -- * Node-related functions
    emptyNode
  , initialNode
  , applyId
  , applyPath
  -- * Path-related functions
  , localPath
  , isLocalPath
  , isValidPath
  , narrowPath
  , narrowSet
  ) where

import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Text       as T

import           Forest.Api

emptyNode :: T.Text -> Bool -> Bool -> Bool -> Bool -> Node
emptyNode text edit delete reply act = Node text edit delete reply act Map.empty

initialNode :: Node
initialNode = emptyNode "Loading..." False False False False

applyId :: NodeId -> Node -> Maybe Node
applyId nodeId node = nodeChildren node Map.!? nodeId

applyPath :: Path -> Node -> Maybe Node
applyPath (Path ids) node = foldM (flip applyId) node ids

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
