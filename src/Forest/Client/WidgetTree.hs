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
import           Control.Monad.Trans.Reader
import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty

data WidgetTree n = WidgetTree (Widget n) [WidgetTree n]

-- This attempts to properly indent multi-line widgets, though it's kinda hacky.
-- It seems to work though, so I'm not going to complain (until the first bugs
-- appear, that is).
--
-- The text strings passed MUST NOT be multiline strings, or this entire
-- function will break.
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
          leftText = vBox $ txt firstLine : replicate (resultHeight - 1) (txt otherLines)
      render $ leftText <+> wrapped

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
