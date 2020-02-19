{-# LANGUAGE OverloadedStrings #-}

module Forest.Client.UiState
  ( UiState
  , newUiState
  , getFocusedPath
  , getFocusedNode
  -- * Modifying the UI state
  , replaceRootNode
  , moveFocusUp
  , moveFocusDown
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
import           Data.Maybe
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Graphics.Vty             as Vty

import           Forest.Client.NodeEditor
import           Forest.Client.NodeUtil
import           Forest.Client.WidgetTree
import           Forest.Node
import qualified Forest.OrderedMap        as OMap

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

getFocusedPath :: UiState n -> Path
getFocusedPath = uiFocused

getFocusedNode :: UiState n -> Node
getFocusedNode s = fromMaybe rootNode $ applyPath (uiFocused s) rootNode
  where
    rootNode = uiRootNode s

{- Modifying -}

-- | Only keep those unfolded nodes that actually exist.
validateUnfolded :: UiState n -> UiState n
validateUnfolded s =
  s {uiUnfolded = Set.filter (referencedNodeExists $ uiRootNode s) (uiUnfolded s)}

-- | Try to find the closest parent to a 'Path' that exists in the 'Node'.
findValidParent :: Node -> Path -> Path
findValidParent _ (Path []) = Path []
findValidParent node (Path (x:xs)) = case applyId x node of
  Nothing    -> Path []
  Just child -> Path [x] <> findValidParent child (Path xs)

-- | Modify the focused path so it always points to an existing node.
validateFocused :: UiState n -> UiState n
validateFocused s =
  let foldedRootNode = applyFolds (uiUnfolded s) (uiRootNode s)
  in  s {uiFocused = findValidParent foldedRootNode $ uiFocused s}

-- | Close the editor if it doesn't point to a valid path.
validateEditor :: UiState n -> UiState n
validateEditor s = case uiEditor s of
  Nothing -> s
  Just e -> keepEditor $ fromMaybe False $ do
    node <- applyPath (eiPath e) (uiRootNode s)
    let flags = nodeFlags node
    pure $ if eiReply e then flagReply flags else flagEdit flags
  where
    keepEditor True  = s
    keepEditor False = s {uiEditor = Nothing}

-- | Modify the UI state so it is consistent again.
validate :: UiState n -> UiState n
validate = validateEditor . validateFocused . validateUnfolded

replaceRootNode :: Node -> UiState n -> UiState n
replaceRootNode node s = validate s {uiRootNode = node}

moveFocusUp :: UiState n -> UiState n
moveFocusUp s =
  let foldedRootNode = applyFolds (uiUnfolded s) (uiRootNode s)
  in  s {uiFocused = findPrevNode foldedRootNode $ uiFocused s}

moveFocusDown :: UiState n -> UiState n
moveFocusDown s =
  let foldedRootNode = applyFolds (uiUnfolded s) (uiRootNode s)
  in  s {uiFocused = findNextNode foldedRootNode $ uiFocused s}

foldAtFocus :: UiState n -> UiState n
foldAtFocus s = validateUnfolded $ s {uiUnfolded = Set.delete (uiFocused s) (uiUnfolded s)}

unfoldAtFocus :: UiState n -> UiState n
unfoldAtFocus s = validateUnfolded $ s {uiUnfolded = Set.insert (uiFocused s) (uiUnfolded s)}

toggleFoldAtFocus :: UiState n -> UiState n
toggleFoldAtFocus s = if uiFocused s `Set.member` uiUnfolded s
  then foldAtFocus s
  else unfoldAtFocus s

editNode :: Bool -> Path -> UiState n -> UiState n
editNode reply path s =
  let text = if reply then "" else nodeText $ getFocusedNode s
      editorInfo = EditorInfo
        { eiEditor = beginEdit (uiEditorName s) text
        , eiPath   = path
        , eiReply  = reply
        }
  in  validateEditor $ s {uiEditor = Just editorInfo}

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

-- | Return an action to update the editor if the editor is currently active.
-- Returns 'Nothing' otherwise.
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
finishEditing s = case uiEditor s of
  Nothing -> (s, Nothing)
  Just e ->
    let editResult = EditResult
          { erText = getCurrentText $ eiEditor e
          , erPath = eiPath e
          , erReply = eiReply e
          }
    in  (abortEditing s, Just editResult)

abortEditing :: UiState n -> UiState n
abortEditing s = s {uiEditor = Nothing}

{- Rendering -}

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

renderNode :: Bool -> Node -> Widget n
renderNode focused node =
  decorateFlags (nodeFlags node) $
  decorateFocus focused $
  decorateExpand (not $ OMap.null $ nodeChildren node) $
  txtWrap $ nodeText node

nodeToTree :: (Ord n, Show n) => UiState n -> Path -> Node -> Maybe [WidgetTree n] -> WidgetTree n
nodeToTree s path node maybeChildren = case uiEditor s of
  Nothing ->
    let isFocused = path == uiFocused s
    in  WidgetTree (renderNode isFocused node) children
  Just e ->
    let renderedEditor = renderNodeEditor $ eiEditor e
        renderedEditorTree = WidgetTree renderedEditor []
    in  if path /= eiPath e
        then WidgetTree (renderNode False node) children
        else if eiReply e
             then WidgetTree (renderNode False node) $ children ++ [renderedEditorTree]
             else WidgetTree renderedEditor children
  where
    children = fromMaybe [] maybeChildren

renderUiState :: (Ord n, Show n) => IndentOptions -> UiState n -> Widget n
renderUiState opts s =
  renderWidgetTree opts $
  foldVisibleNodes (nodeToTree s) (uiUnfolded s) (uiRootNode s)
