{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.WidgetTree
  ( WidgetTree(..)
  , renderWidgetTree
  , IndentOptions(..)
  , boxDrawingBranching
  , boxDrawingLine
  , asciiBranching
  , asciiLine
  ) where

import           Brick
import           Brick.BorderMap
import           Control.Monad.Trans.Reader
import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty

data WidgetTree n = WidgetTree (Widget n) [WidgetTree n]

addLoc :: Location -> Location -> Location
addLoc l1 l2 =
  let (x1, y1) = loc l1
      (x2, y2) = loc l2
  in  Location (x1 + x2, y1 + y2)

offsetResult :: Location -> Result n -> Result n
offsetResult offset result = result
  { cursors = map offsetCursor $ cursors result
  , visibilityRequests = map offsetVr $ visibilityRequests result
  , extents = map offsetExtent $ extents result
  , borders = translate offset $ borders result
  }
  where
    offsetCursor c = c{cursorLocation = addLoc offset $ cursorLocation c}
    offsetVr vr = vr{vrPosition = addLoc offset $ vrPosition vr}
    offsetExtent e = e
      { extentUpperLeft = addLoc offset $ extentUpperLeft e
      , extentOffset    = addLoc offset $ extentOffset e
      }

indentWith :: T.Text -> T.Text -> Widget n -> Widget n
indentWith firstLine otherLines wrapped = Widget
  { hSize = hSize wrapped
  , vSize = vSize wrapped
  , render = renderWidget
  }
  where
    maxWidth = max (T.length firstLine) (T.length otherLines)
    renderWidget = do
      context <- ask
      result <- render $ hLimit (availWidth context - maxWidth) wrapped
      let resultHeight = Vty.imageHeight $ image result
          textLines = firstLine : replicate (resultHeight - 1) otherLines
          leftImage = Vty.vertCat $ map (Vty.text' Vty.defAttr) textLines
          newImage = leftImage Vty.<|> image result
          newResult = offsetResult (Location (maxWidth, 0)) $ result{image=newImage}
      pure newResult

indent :: IndentOptions -> [Widget n] -> Widget n
indent opts widgets = vBox $ reverse $ case reverse widgets of
  [] -> []
  (w:ws) ->
    indentWith (lastBranch opts) (afterLastBranch opts) w :
    map (indentWith (inlineBranch opts) (noBranch opts)) ws

renderWidgetTree :: IndentOptions -> WidgetTree n -> Widget n
renderWidgetTree opts (WidgetTree node children) =
  node <=> indent opts (map (renderWidgetTree opts) children)

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
