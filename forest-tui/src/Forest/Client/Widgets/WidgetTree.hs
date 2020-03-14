{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.Widgets.WidgetTree
  ( WidgetTree(..)
  , renderWidgetTreeWith
  , renderWidgetTree
  , treeLineAttr
  , IndentOptions(..)
  , boxDrawingBranching
  , boxDrawingLine
  , asciiBranching
  , asciiLine
  ) where

import           Brick
import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty

data WidgetTree n = WidgetTree (Widget n) [WidgetTree n]

indentWith :: AttrName -> T.Text -> T.Text -> Widget n -> Widget n
-- The "left" variables are for rendering the indentation text, the "right"
-- variables are for the rendered wrapped widget.
indentWith indentAttrName firstLine otherLines wrapped =
  Widget (hSize wrapped) (vSize wrapped) $ do
    let leftWidth = max (T.length firstLine) (T.length otherLines)
    context <- getContext
    rightResult <- render $ hLimit (availWidth context - leftWidth) wrapped
    let rightImage = image rightResult
        -- Construct the Vty image containing the indentation text
        height = Vty.imageHeight rightImage
        leftLines = firstLine : replicate (height - 1) otherLines
        leftAttribute = attrMapLookup indentAttrName $ ctxAttrMap context
        leftImage = Vty.vertCat $ map (Vty.text' leftAttribute) leftLines
        -- Add the indentation text to the left of the result image
        combinedImage = leftImage Vty.<|> image rightResult
        offset = Location (leftWidth, 0)
        result = (addResultOffset offset rightResult) {image=combinedImage}
    pure result

indent :: AttrName -> IndentOptions -> [Widget n] -> Widget n
indent indentAttrName opts widgets = vBox $ reverse $ case reverse widgets of
  [] -> []
  (w:ws) ->
    indentWith indentAttrName (indentLastNodeFirstLine opts) (indentLastNodeRest opts) w :
    map (indentWith indentAttrName (indentNodeFirstLine opts) (indentNodeRest opts)) ws

renderWidgetTreeWith :: AttrName -> IndentOptions -> WidgetTree n -> Widget n
renderWidgetTreeWith indentAttrName opts (WidgetTree node children) =
  node <=> indent indentAttrName opts (map (renderWidgetTreeWith indentAttrName opts) children)

renderWidgetTree :: IndentOptions -> WidgetTree n -> Widget n
renderWidgetTree = renderWidgetTreeWith treeLineAttr

-- | The attribute that 'renderWidgetTree' uses.
treeLineAttr :: AttrName
treeLineAttr = "treeLine"

-- | These options control how a tree is rendered.
--
-- In the following example, the indent options are set to @'IndentOptions' "a" "b" "c" "d"@:
--
-- > a This is the first node.
-- > b c It has a child.
-- > a This is a...
-- > b multiline...
-- > b node.
-- > c This is the last node.
-- > d c It has one child.
-- > d c And another one.
--
-- Warning: The options /must/ be single line strings and /must not/ contain
-- newlines of any sort.
data IndentOptions = IndentOptions
  { indentNodeFirstLine     :: T.Text
  -- ^ This is prepended to the first line of a node.
  , indentNodeRest          :: T.Text
  -- ^ This is prepended to all other lines of a node, including its subnodes.
  , indentLastNodeFirstLine :: T.Text
  -- ^ This is prepended to the first line of the last node.
  , indentLastNodeRest      :: T.Text
  -- ^ This is prepended to all other lines of the last node, including its subnodes.
  } deriving (Show, Eq)

boxDrawingBranching :: IndentOptions
boxDrawingBranching = IndentOptions
  { indentNodeFirstLine     = "├╴"
  , indentNodeRest          = "│ "
  , indentLastNodeFirstLine = "└╴"
  , indentLastNodeRest      = "  "
  }

boxDrawingLine :: IndentOptions
boxDrawingLine = IndentOptions
  { indentNodeFirstLine     = "│ "
  , indentNodeRest          = "│ "
  , indentLastNodeFirstLine = "│ "
  , indentLastNodeRest      = "│ "
  }

asciiBranching :: IndentOptions
asciiBranching = IndentOptions
  { indentNodeFirstLine     = "+-"
  , indentNodeRest          = "| "
  , indentLastNodeFirstLine = "+-"
  , indentLastNodeRest      = "  "
  }

asciiLine :: IndentOptions
asciiLine = IndentOptions
  { indentNodeFirstLine     = "| "
  , indentNodeRest          = "| "
  , indentLastNodeFirstLine = "| "
  , indentLastNodeRest      = "| "
  }
