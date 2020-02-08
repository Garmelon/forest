{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.NodeEditor
  ( NodeEditor
  , asReply
  , editNode
  , replyToNode
  , handleNodeEditorEvent
  , renderNodeEditor
  ) where

import           Brick
import           Brick.Widgets.Edit
import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty

import           Forest.Client.ResourceName

data NodeEditor = NodeEditor
  { neEditor :: Editor T.Text ResourceName
  , neReply  :: Bool
  } deriving (Show)

asReply :: NodeEditor -> Bool
asReply = neReply

editNode :: T.Text -> NodeEditor
editNode text = NodeEditor
  { neEditor = editorText RnEditor (Just 1) text
  , neReply = False
  }

replyToNode :: NodeEditor
replyToNode = NodeEditor
  { neEditor = editorText RnEditor (Just 1) ""
  , neReply = True
  }

handleNodeEditorEvent :: Vty.Event -> NodeEditor -> EventM ResourceName NodeEditor
handleNodeEditorEvent event es = do
  newEditor <- handleEditorEvent event $ neEditor es
  pure es{neEditor = newEditor}

renderNodeEditor :: NodeEditor -> Widget ResourceName
renderNodeEditor es = renderEditor renderFunc True $ neEditor es
  where
    renderFunc :: [T.Text] -> Widget ResourceName
    renderFunc = vBox . map txt
