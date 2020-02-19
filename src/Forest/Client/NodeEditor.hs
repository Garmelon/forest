{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.NodeEditor
  ( NodeEditor
  , getCurrentText
  , beginEdit
  , handleNodeEditorEvent
  , renderNodeEditor
  ) where

import           Brick
import           Brick.Widgets.Edit
import qualified Data.Text          as T
import           Data.Text.Zipper
import qualified Graphics.Vty       as Vty
import           Lens.Micro

newtype NodeEditor n = NodeEditor (Editor T.Text n)
  deriving (Show)

getCurrentLines :: NodeEditor n -> [T.Text]
getCurrentLines (NodeEditor e) = getEditContents e

getCurrentText :: NodeEditor n -> T.Text
getCurrentText = T.intercalate "\n" . getCurrentLines

beginEdit :: n -> T.Text -> NodeEditor n
beginEdit name = NodeEditor . applyEdit gotoEOL . editorText name Nothing

edit :: (TextZipper T.Text -> TextZipper T.Text) -> NodeEditor n -> EventM n (NodeEditor n)
edit z (NodeEditor e) = pure $ NodeEditor $ applyEdit z e

handleNodeEditorEvent :: Vty.Event -> NodeEditor n -> EventM n (NodeEditor n)
handleNodeEditorEvent (Vty.EvKey Vty.KHome _) ne = edit gotoBOL ne
handleNodeEditorEvent (Vty.EvKey Vty.KEnd _) ne = edit gotoEOL ne
handleNodeEditorEvent event (NodeEditor e) = NodeEditor <$> handleEditorEvent event e

renderLines :: [T.Text] -> Widget n
renderLines = vBox . map (\t -> txt $ if T.null t then " " else t)

renderNodeEditor :: (Ord n, Show n) => NodeEditor n -> Widget n
renderNodeEditor ne@(NodeEditor e) =
  makeVisible $ vLimit height $ renderEditor renderLines True e
  where
    height = length $ getCurrentLines ne
    (row, col) = cursorPosition $ e ^. editContentsL
    makeVisible = visibleRegion (Location (col, row)) (1, 1)
