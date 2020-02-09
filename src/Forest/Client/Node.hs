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

decorateExpand :: Bool -> Widget n -> Widget n
decorateExpand True widget  = withDefAttr "expand" widget
decorateExpand False widget = withDefAttr "noexpand" widget

decorateFocus :: Bool -> Widget n -> Widget n
decorateFocus True widget  = withDefAttr "focus" widget
decorateFocus False widget = withDefAttr "nofocus" widget

decorateFlags :: Node -> Widget n -> Widget n
decorateFlags node widget =
  let e = if nodeEdit node then "e" else "-"
      d = if nodeDelete node then "d" else "-"
      r = if nodeReply node then "r" else "-"
      a = if nodeAct node then "a" else "-"
      flags = "(" <> e <> d <> r <> a <> ")"
  in  widget <+> txt " " <+> withDefAttr "flags" (txt flags)

narrowDrawState :: NodeId -> DrawState -> DrawState
narrowDrawState nodeId ds = ds
  { dsUnfolded = narrowSet nodeId $ dsUnfolded ds
  , dsFocused = narrowPath nodeId =<< dsFocused ds
  }

nodeToWidget :: Node -> Widget ResourceName
nodeToWidget  node = txt $ nodeText node

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
    | otherwise  -> WidgetTree (expand $ renderNodeEditor ed) subnodeWidgets
  where
    focused = isFocused ds
    folded = isFolded ds
    expand = decorateExpand $ hasChildren node
    nodeWidget =
      decorateFlags node $
      decorateFocus focused $
      expand $ nodeToWidget node
    subnodeWidgets = if folded then [] else subnodesToTrees ds node
