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
import qualified Forest.OrderedMap          as OMap

data DrawState = DrawState
  { dsEditor   :: Maybe NodeEditor
  , dsFocused  :: Maybe Path
  , dsUnfolded :: Set.Set Path
  }

isFocused :: DrawState -> Bool
isFocused ds = dsFocused ds == Just mempty

isFolded :: DrawState -> Bool
isFolded ds = not $ mempty `Set.member` dsUnfolded ds

decorateExpand :: Bool -> Widget n -> Widget n
decorateExpand True widget  = withDefAttr "expand" widget
decorateExpand False widget = withDefAttr "noexpand" widget

decorateFocus :: Bool -> Widget n -> Widget n
decorateFocus True widget  = visible $ withDefAttr "focus" widget
decorateFocus False widget = withDefAttr "nofocus" widget

decorateFlags :: NodeFlags -> Widget n -> Widget n
decorateFlags node widget =
  let e = if flagEdit node then "e" else "-"
      d = if flagDelete node then "d" else "-"
      r = if flagReply node then "r" else "-"
      a = if flagAct node then "a" else "-"
      flags = "(" <> e <> d <> r <> a <> ")"
  in  widget <+> txt " " <+> withDefAttr "flags" (txt flags)

narrowDrawState :: NodeId -> DrawState -> DrawState
narrowDrawState nodeId ds = ds
  { dsUnfolded = narrowSet nodeId $ dsUnfolded ds
  , dsFocused = narrow nodeId =<< dsFocused ds
  }

nodeToWidget :: Node -> Widget ResourceName
nodeToWidget  node = txtWrap $ nodeText node

subnodeToTree :: DrawState -> NodeId -> Node -> WidgetTree ResourceName
subnodeToTree ds nodeId node =
  let newDs = narrowDrawState nodeId ds
  in  nodeToTree newDs node

subnodesToTrees :: DrawState -> Node -> [WidgetTree ResourceName]
subnodesToTrees ds = map (uncurry $ subnodeToTree ds) . OMap.toList . nodeChildren

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
      decorateFlags (nodeFlags node) $
      decorateFocus focused $
      expand $ nodeToWidget node
    subnodeWidgets = if folded then [] else subnodesToTrees ds node
