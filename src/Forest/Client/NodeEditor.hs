{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.NodeEditor
  ( NodeEditor
  , getCurrentText
  , asReply
  , editNode
  , replyToNode
  , handleNodeEditorEvent
  , renderNodeEditor
  ) where

import           Brick
import           Brick.Widgets.Edit
import qualified Data.Text                  as T
import           Data.Text.Zipper
import qualified Graphics.Vty               as Vty
import           Lens.Micro

import           Forest.Client.ResourceName

data NodeEditor = NodeEditor
  { neEditor :: Editor T.Text ResourceName
  , neReply  :: Bool
  } deriving (Show)

getCurrentText :: NodeEditor -> [T.Text]
getCurrentText = getEditContents . neEditor

asReply :: NodeEditor -> Bool
asReply = neReply

editNode :: T.Text -> NodeEditor
editNode text = NodeEditor
  { neEditor = applyEdit gotoEOL $ editorText RnEditor Nothing text
  , neReply = False
  }

replyToNode :: NodeEditor
replyToNode = NodeEditor
  { neEditor = editorText RnEditor Nothing ""
  , neReply = True
  }

edit :: (TextZipper T.Text -> TextZipper T.Text) -> NodeEditor -> EventM ResourceName NodeEditor
edit z ne = pure $ ne{neEditor = applyEdit z $ neEditor ne}

handleNodeEditorEvent :: Vty.Event -> NodeEditor -> EventM ResourceName NodeEditor
handleNodeEditorEvent (Vty.EvKey Vty.KHome _) ne = edit gotoBOL ne
handleNodeEditorEvent (Vty.EvKey Vty.KEnd _) ne = edit gotoEOL ne
handleNodeEditorEvent event ne = do
  newEditor <- handleEditorEvent event $ neEditor ne
  pure ne{neEditor = newEditor}

renderNodeEditor :: NodeEditor -> Widget ResourceName
renderNodeEditor ne = makeVisible $ vLimit height $ renderEditor renderFunc True ed
  where
    ed = neEditor ne

    height = length $ getCurrentText ne
    renderFunc :: [T.Text] -> Widget ResourceName
    renderFunc = vBox . map (\t -> if T.null t then txt " " else txt t)

    (row, col) = cursorPosition $ ed ^. editContentsL
    makeVisible = visibleRegion (Location (col, row)) (1, 1)
