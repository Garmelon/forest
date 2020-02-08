{-# LANGUAGE OverloadedStrings #-}

module Forest.Tree
  ( emptyNode
  , initialNode
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import           Forest.Api

emptyNode :: T.Text -> Bool -> Bool -> Bool -> Bool -> Node
emptyNode text edit delete reply act = Node text edit delete reply act Map.empty

initialNode :: Node
initialNode = emptyNode "Loading..." False False False False
