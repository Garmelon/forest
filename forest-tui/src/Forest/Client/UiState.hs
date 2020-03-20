{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.UiState
  ( UiState
  , newUiState
  , focusedPath
  , focusedNode
  -- * Modifying the UI state
  , replaceRootNode
  , moveFocusUp
  , moveFocusDown
  , moveFocusToParent
  , moveFocusToPrevSibling
  , moveFocusToNextSibling
  , moveFocusToTop
  , moveFocusToBottom
  , foldAtFocus
  , unfoldAtFocus
  , toggleFoldAtFocus
  -- ** The node editor
  -- *** Creating
  , editCurrentNode
  , replyToCurrentNode
  , replyAfterCurrentNode
  -- *** Updating
  , isEditorActive
  , updateEditor
  -- *** Finishing the edit
  , EditResult(..)
  , finishEditing
  , abortEditing
  -- * Rendering the UI state
  , renderUiState
  ) where

import           Brick
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Graphics.Vty                     as Vty

import           Forest.Client.NodeUtil
import           Forest.Client.Widgets.NodeEditor
import           Forest.Client.Widgets.WidgetTree
import           Forest.Node
import qualified Forest.OrderedMap                as OMap

data EditorInfo n = EditorInfo
  { eiEditor :: !(NodeEditor n)
  , eiPath   :: !Path
  , eiReply  :: !Bool
  } deriving (Show)

data UiState n = UiState
  { uiRootNode   :: !Node
  , uiFocused    :: !Path
  , uiUnfolded   :: !Unfolded
  , uiEditor     :: !(Maybe (EditorInfo n))
  , uiEditorName :: !n
  } deriving (Show)

newUiState :: n -> Node -> UiState n
newUiState editorName node = UiState
  { uiRootNode   = node
  , uiFocused    = mempty
  , uiUnfolded   = mempty
  , uiEditor     = Nothing
  , uiEditorName = editorName
  }

focusedPath :: UiState n -> Path
focusedPath = uiFocused

focusedNode :: UiState n -> Node
focusedNode s = fromMaybe rootNode $ applyPath rootNode $ uiFocused s
  where
    rootNode = uiRootNode s

foldedRootNode :: UiState n -> Node
foldedRootNode s = applyFolds (uiUnfolded s) (uiRootNode s)

{- Modifying -}

-- | Only keep those unfolded nodes that actually exist.
validateUnfolded :: UiState n -> UiState n
validateUnfolded s =
  s {uiUnfolded = Set.filter (referencedNodeExists $ uiRootNode s) (uiUnfolded s)}

-- | Try to find the closest parent to a 'Path' that exists in the 'Node'.
findValidParent :: Node -> Path -> Path
findValidParent _ (Path []) = Path []
findValidParent node (Path (x:xs)) = case applyId node x of
  Nothing    -> Path []
  Just child -> Path [x] <> findValidParent child (Path xs)

-- | Move to the closest valid parent as a last-ditch effort if the current
-- focus path is invalid.
validateFocused :: UiState n -> UiState n
validateFocused s =
  s {uiFocused = findValidParent (foldedRootNode s) (uiFocused s)}

-- | Close the editor if it doesn't point to a valid path.
validateEditor :: UiState n -> UiState n
validateEditor s = fromMaybe s{uiEditor = Nothing} $ do
  e <- uiEditor s
  node <- applyPath (uiRootNode s) (eiPath e)
  let flags = nodeFlags node
  guard $ if eiReply e then flagReply flags else flagEdit flags
  pure s

-- | Modify the UI state so it is consistent again.
validate :: UiState n -> UiState n
validate = validateEditor . validateFocused . validateUnfolded

-- | Find a node that is close to the previously focused node, taking into
-- account its previous position in the tree.
findNextValidNode :: Node -> Node -> Path -> Path
findNextValidNode _ _ (Path []) = Path []
findNextValidNode from to (Path (x:xs)) = fromMaybe (Path []) $ do
  fromNode <- applyId from x
  case applyId to x of
    Just toNode -> pure $ Path [x] <> findNextValidNode fromNode toNode (Path xs)
    Nothing -> do
      fromIdx <- elemIndex x $ OMap.keys $ nodeChildren from
      let toKeys = OMap.keys $ nodeChildren to
      x' <- getValueClosestToIndex fromIdx toKeys
      pure $ Path [x']
  where
    -- Slightly unsafe code, but it should be fine
    getValueClosestToIndex idx list
      | length list > idx = Just $ list !! idx
      | null list         = Nothing
      | otherwise         = Just $ last list

replaceRootNode :: Node -> UiState n -> UiState n
replaceRootNode node s = validate s
  { uiRootNode = node
  , uiFocused  = findNextValidNode (uiRootNode s) node (uiFocused s)
  }

moveFocus :: (Node -> Path -> Maybe Path) -> UiState n -> UiState n
moveFocus f s = fromMaybe s $ do
  newFocus <- f (foldedRootNode s) (uiFocused s)
  pure $ validateFocused s{uiFocused = newFocus}

moveFocusUp :: UiState n -> UiState n
moveFocusUp = moveFocus prevNode

moveFocusDown :: UiState n -> UiState n
moveFocusDown = moveFocus nextNode

moveFocusToPrevSibling :: UiState n -> UiState n
moveFocusToPrevSibling = moveFocus prevSibling

moveFocusToNextSibling :: UiState n -> UiState n
moveFocusToNextSibling = moveFocus nextSibling

moveFocusToParent :: UiState n -> UiState n
moveFocusToParent = moveFocus $ const parent

moveFocusToTop :: UiState n -> UiState n
moveFocusToTop = moveFocus firstNode

moveFocusToBottom :: UiState n -> UiState n
moveFocusToBottom = moveFocus lastNode

foldAtFocus :: UiState n -> UiState n
foldAtFocus s =
  validateUnfolded s{uiUnfolded = Set.delete (uiFocused s) (uiUnfolded s)}

unfoldAtFocus :: UiState n -> UiState n
unfoldAtFocus s =
  validateUnfolded s{uiUnfolded = Set.insert (uiFocused s) (uiUnfolded s)}

toggleFoldAtFocus :: UiState n -> UiState n
toggleFoldAtFocus s = if uiFocused s `Set.member` uiUnfolded s
  then foldAtFocus s
  else unfoldAtFocus s

editNode :: Bool -> Path -> UiState n -> UiState n
editNode reply path s =
  let text = if reply then "" else nodeText $ focusedNode s
      editorInfo = EditorInfo
        { eiEditor = beginEdit (uiEditorName s) text
        , eiPath   = path
        , eiReply  = reply
        }
  in  validateEditor s{uiEditor = Just editorInfo}

-- | Begin editing the currently focused node. Discards any current editor
-- status.
editCurrentNode :: UiState n -> UiState n
editCurrentNode s = editNode False (uiFocused s) s

-- | Reply to the currently focused node. Discards any current editor status.
replyToCurrentNode :: UiState n -> UiState n
replyToCurrentNode s = editNode True (uiFocused s) s

-- | Reply in parallel to the currently focused node, unless it is the root node
-- (in which case no action is taken).
replyAfterCurrentNode :: UiState n -> UiState n
replyAfterCurrentNode s = case parent $ uiFocused s of
  Nothing   -> s
  Just path -> editNode True path s

isEditorActive :: UiState n -> Bool
isEditorActive = isJust . uiEditor

updateEditor :: Vty.Event -> UiState n -> EventM n (UiState n)
updateEditor ev s = case uiEditor s of
  Nothing -> pure s
  Just e -> do
    newEditor <- handleNodeEditorEvent ev $ eiEditor e
    pure s {uiEditor = Just e {eiEditor = newEditor}}

data EditResult = EditResult
  { erText  :: T.Text
  , erPath  :: Path
  , erReply :: Bool
  } deriving (Show)

finishEditing :: UiState n -> (UiState n, Maybe EditResult)
finishEditing s = fromMaybe (s, Nothing) $ do
  e <- uiEditor s
  let editResult = EditResult
        { erText  = getCurrentText $ eiEditor e
        , erPath  = eiPath e
        , erReply = eiReply e
        }
  pure (abortEditing s, Just editResult)

abortEditing :: UiState n -> UiState n
abortEditing s = s{uiEditor = Nothing}

{- Rendering -}

decorateExpand :: Bool -> Widget n -> Widget n
decorateExpand True  = withDefAttr "expand"
decorateExpand False = id

decorateFocus :: Bool -> Widget n -> Widget n
decorateFocus True = withDefAttr "focus"
decorateFocus False = id

decorateFlags :: NodeFlags -> Widget n -> Widget n
decorateFlags node widget =
  let e = if flagEdit node then "e" else "-"
      d = if flagDelete node then "d" else "-"
      r = if flagReply node then "r" else "-"
      a = if flagAct node then "a" else "-"
      flags = "(" <> e <> d <> r <> a <> ")"
  in  widget <+> txt " " <+> withDefAttr "flags" (txt flags)

renderNode :: Bool -> Node -> Widget n
renderNode focused node =
  decorateFlags (nodeFlags node) $
  decorateFocus focused $
  decorateExpand (hasChildren node) $
  padRight Max text
  where
    -- The height of the text widget must be at least 1 for 'padRight Max' to
    -- expand it. As far as I know, if the text has at least one character, it
    -- also has a height of at least 1, but if it has no characters, its height
    -- is 0. Because of that, we insert a filler space if the text is empty.
    text
      | T.null $ nodeText node = txt " "
      | otherwise              = txtWrap $ nodeText node

nodeToTree
  :: (Ord n, Show n)
  => UiState n
  -> Path
  -> Node
  -> Maybe [WidgetTree n]
  -> WidgetTree n
nodeToTree s path node maybeChildren = case uiEditor s of
  Just e | path == eiPath e ->
    let renderedEditor = renderNodeEditor $ eiEditor e
    in if eiReply e
       then WidgetTree renderedNode $ children ++ [WidgetTree renderedEditor []]
       else WidgetTree renderedEditor children
  _ -> WidgetTree (visible renderedNode) children
  where
    renderedNode = renderNode (path == uiFocused s) node
    children = fromMaybe [] maybeChildren

renderUiState :: (Ord n, Show n) => IndentOptions -> UiState n -> Widget n
renderUiState opts s =
  renderWidgetTree opts $
  foldVisibleNodes (nodeToTree s) (uiUnfolded s) (uiRootNode s)
