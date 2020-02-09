{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.Node
  ( DrawState(..)
  , nodeToTree
  ) where

import           Brick
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

import           Forest.Client.NodeEditor
import           Forest.Client.ResourceName
import           Forest.Client.WidgetTree
import           Forest.Node

data DrawState = DrawState
  { dsEditor   :: Maybe NodeEditor
  , dsFocused  :: Maybe Path
  , dsUnfolded :: Set.Set Path
  }

isFocused :: DrawState -> Bool
isFocused ds = (isLocalPath <$> dsFocused ds) == Just True

narrowDrawState :: NodeId -> DrawState -> DrawState
narrowDrawState nodeId ds = ds
  { dsUnfolded = narrowSet nodeId $ dsUnfolded ds
  , dsFocused = narrowPath nodeId =<< dsFocused ds
  }

nodeToWidget :: Bool -> Node -> Widget ResourceName
nodeToWidget focused node =
  let nodeWidget = txt $ nodeText node
      expandStyle = if null (nodeChildren node) then "noexpand" else "expand"
      focusStyle = if focused then "focus" else "nofocus"
  in  withAttr focusStyle $ withAttr expandStyle nodeWidget

subnodeToTree :: NodeId -> DrawState -> Node -> WidgetTree ResourceName
subnodeToTree  nodeId ds node =
  let newDs = narrowDrawState nodeId ds
  in  nodeToTree newDs node

subnodesToTrees :: DrawState -> Map.Map NodeId Node -> [WidgetTree ResourceName]
subnodesToTrees ds nodes =
  map (\(nodeId, node) -> subnodeToTree nodeId ds node) $
  Map.toAscList nodes

nodeToTree :: DrawState -> Node -> WidgetTree ResourceName
nodeToTree ds node = case dsEditor ds of
  Nothing -> WidgetTree nodeWidget subnodeWidgets
  Just ed
    | asReply ed -> WidgetTree nodeWidget (subnodeWidgets ++ [WidgetTree (renderNodeEditor ed) []])
    | otherwise  -> WidgetTree (renderNodeEditor ed) subnodeWidgets
  where
    focused = isFocused ds
    nodeWidget = nodeToWidget focused node
    subnodeWidgets = subnodesToTrees ds $ nodeChildren node
