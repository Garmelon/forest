{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.Node
  ( DrawState(..)
  , renderNode
  ) where

import           Brick
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

import           Forest.Client.NodeEditor
import           Forest.Client.ResourceName
import           Forest.Node

data DrawState = DrawState
  { dsEditor   :: Maybe NodeEditor
  , dsFocused  :: Maybe Path
  , dsUnfolded :: Set.Set Path
  }

narrowDrawState :: NodeId -> DrawState -> DrawState
narrowDrawState nodeId ds = ds
  { dsUnfolded = narrowSet nodeId $ dsUnfolded ds
  , dsFocused = narrowPath nodeId =<< dsFocused ds
  }

indent :: Widget n -> Widget n
indent = (txt "| " <+>)

drawSubnode :: NodeId -> DrawState -> Node -> Widget ResourceName
drawSubnode nodeId ds node =
  let newDs = narrowDrawState nodeId ds
  in  indent $ renderNode newDs node

drawSubnodes :: DrawState -> Map.Map NodeId Node -> Widget ResourceName
drawSubnodes ds nodes = vBox $
  map (\(nodeId, node) -> drawSubnode nodeId ds node) $
  Map.toAscList nodes

isFocused :: DrawState -> Bool
isFocused ds = (isLocalPath <$> dsFocused ds) == Just True

drawNodeWithoutEditor :: DrawState -> Node -> Widget ResourceName
drawNodeWithoutEditor ds node
  | isFocused ds = withAttr "focused" nodeWidget <=> subnodesWidget
  | otherwise = nodeWidget <=> subnodesWidget
  where
    nodeWidget = txt $ nodeText node
    subnodesWidget = drawSubnodes ds $ nodeChildren node

drawNodeWithEditor :: NodeEditor -> DrawState -> Node -> Widget ResourceName
drawNodeWithEditor ed ds node
  | asReply ed = drawNodeWithoutEditor ds node <=> indent (renderNodeEditor ed)
  | otherwise = renderNodeEditor ed <=> drawSubnodes ds (nodeChildren node)

renderNode :: DrawState -> Node -> Widget ResourceName
renderNode ds node
  | isFocused ds = case dsEditor ds of
      Nothing -> drawNodeWithoutEditor ds node
      Just ed -> drawNodeWithEditor ed ds node
  | otherwise = drawNodeWithoutEditor ds node
