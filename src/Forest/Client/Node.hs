{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.Node
  ( DrawState(..)
  , nodeToTree
  ) where

import           Brick
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

isFolded :: DrawState -> Bool
isFolded ds = not $ localPath `Set.member` dsUnfolded ds

narrowDrawState :: NodeId -> DrawState -> DrawState
narrowDrawState nodeId ds = ds
  { dsUnfolded = narrowSet nodeId $ dsUnfolded ds
  , dsFocused = narrowPath nodeId =<< dsFocused ds
  }

nodeToWidget :: Bool -> Node -> Widget ResourceName
nodeToWidget focused node =
  let nodeWidget = txt $ nodeText node
      expandStyle = if hasChildren node then "expand" else "noexpand"
      focusStyle = if focused then "focus" else "nofocus"
  in  withDefAttr focusStyle $ withDefAttr expandStyle nodeWidget

subnodeToTree :: DrawState -> NodeId -> Node -> WidgetTree ResourceName
subnodeToTree ds nodeId node =
  let newDs = narrowDrawState nodeId ds
  in  nodeToTree newDs node

subnodesToTrees :: DrawState -> Node -> [WidgetTree ResourceName]
subnodesToTrees ds = mapChildren (subnodeToTree ds)

nodeToTree :: DrawState -> Node -> WidgetTree ResourceName
nodeToTree ds node = case dsEditor ds of
  Nothing -> WidgetTree nodeWidget subnodeWidgets
  Just ed
    | not focused -> WidgetTree nodeWidget subnodeWidgets
    | asReply ed -> WidgetTree nodeWidget (subnodeWidgets ++ [WidgetTree (renderNodeEditor ed) []])
    | otherwise  -> WidgetTree (renderNodeEditor ed) subnodeWidgets
  where
    focused = isFocused ds
    folded = isFolded ds
    nodeWidget = nodeToWidget focused node
    subnodeWidgets = if folded then [] else subnodesToTrees ds node
