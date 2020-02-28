{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.WidgetTree
  ( WidgetTree(..)
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
import           Lens.Micro

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
        leftAttribute = attrMapLookup indentAttrName $ context ^. ctxAttrMapL
        leftImage = Vty.vertCat $ map (Vty.text' leftAttribute) leftLines
        -- Add the indentation text to the left of the result image
        combinedImage = leftImage Vty.<|> image rightResult
        offset = Location (leftWidth, 0)
        result = addResultOffset offset rightResult & imageL .~ combinedImage
    pure result

indent :: IndentOptions -> [Widget n] -> Widget n
indent opts widgets = vBox $ reverse $ case reverse widgets of
  [] -> []
  (w:ws) ->
    indentWith treeLineAttr (lastBranch opts) (afterLastBranch opts) w :
    map (indentWith treeLineAttr (inlineBranch opts) (noBranch opts)) ws

renderWidgetTree :: IndentOptions -> WidgetTree n -> Widget n
renderWidgetTree opts (WidgetTree node children) =
  node <=> indent opts (map (renderWidgetTree opts) children)

treeLineAttr :: AttrName
treeLineAttr = "treeLine"

-- | These options control how a tree is rendered. For more information on how
-- the various options are used, try rendering a tree with 'boxDrawingBranhing'
-- and inspect the results.
--
-- Warning: The options *must* be single line strings and *must not* contain
-- newlines of any sort.
data IndentOptions = IndentOptions
  { noBranch        :: T.Text
  , inlineBranch    :: T.Text
  , lastBranch      :: T.Text
  , afterLastBranch :: T.Text
  } deriving (Show, Eq)

boxDrawingBranching :: IndentOptions
boxDrawingBranching = IndentOptions
  { noBranch        = "│ "
  , inlineBranch    = "├╴"
  , lastBranch      = "└╴"
  , afterLastBranch = "  "
  }

boxDrawingLine :: IndentOptions
boxDrawingLine = IndentOptions
  { noBranch        = "│ "
  , inlineBranch    = "│ "
  , lastBranch      = "│ "
  , afterLastBranch = "│ "
  }

asciiBranching :: IndentOptions
asciiBranching = IndentOptions
  { noBranch        = "| "
  , inlineBranch    = "+-"
  , lastBranch      = "+-"
  , afterLastBranch = "  "
  }

asciiLine :: IndentOptions
asciiLine = IndentOptions
  { noBranch        = "| "
  , inlineBranch    = "| "
  , lastBranch      = "| "
  , afterLastBranch = "| "
  }
