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
  , moveFocusToFirstChild
  , moveFocusToLastChild
  , moveFocusToFirstSibling
  , moveFocusToLastSibling
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
import           Data.List
import           Data.Maybe
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Graphics.Vty                     as Vty
import           Safe

import           Forest.Client.NodeUtil
import           Forest.Client.Widgets.NodeEditor
import           Forest.Client.Widgets.WidgetTree
import           Forest.Node
import qualified Forest.OrderedMap                as OMap
import           Forest.Util

data EditorInfo n = EditorInfo
  { eiEditor :: !(NodeEditor n)
  , eiPath   :: !Path
  , eiReply  :: !Bool
  } deriving (Show)

-- | This type is used to move the cursor to a node that is expected to appear
-- soon. For example, if the user creates a new node by replying, the cursor
-- should move to this new node as soon as it appears (unless the cursor has
-- been moved in-between).
data FocusTarget = FocusTarget
  { ftPath  :: !Path
  -- ^ The node relative to which the target is set
  , ftChild :: !Bool
  -- ^ If this is 'True', the target points towards the node's first child. If
  -- it is 'False', the target points towards the node's next sibling.
  } deriving (Show)

data UiState n = UiState
  { uiRootNode   :: !Node
  , uiFocused    :: !Path
  , uiTarget     :: !(Maybe FocusTarget)
  , uiUnfolded   :: !Unfolded
  , uiEditor     :: !(Maybe (EditorInfo n))
  , uiEditorName :: !n
  } deriving (Show)

newUiState :: n -> Node -> UiState n
newUiState editorName node = UiState
  { uiRootNode   = node
  , uiFocused    = mempty
  , uiTarget     = Nothing
  , uiUnfolded   = mempty
  , uiEditor     = Nothing
  , uiEditorName = editorName
  }

getFocusedPath :: UiState n -> Path
getFocusedPath = uiFocused

getFocusedNode :: UiState n -> Node
getFocusedNode s = fromMaybe rootNode $ applyPath rootNode $ uiFocused s
  where
    rootNode = uiRootNode s

foldedRootNode :: UiState n -> Node
foldedRootNode s = applyFolds (uiUnfolded s) (uiRootNode s)

{- Modifying -}

-- | Only keep those unfolded nodes that actually exist.
validateUnfolded :: UiState n -> UiState n
validateUnfolded s =
  s {uiUnfolded = Set.filter (referencedNodeExists $ uiRootNode s) (uiUnfolded s)}

-- | Try to apply the focus target if it is set and the corresponding node is
-- visible. Does not modify the UI state otherwise.
--
-- The plan is that this does not behave in an unexpected way. It definitely
-- should not move the cursor around if the user does not expect it, because
-- that would be annoying.
--
-- One scenario this tries to avoid: The targeted node exists but is not
-- visible. The cursor is moved to the target node, and since it is not visible,
-- 'moveToValidParent' moves it upwards to the first visible parent. This causes
-- the cursor to jump weirdly and without explanation.
moveToTarget :: UiState n -> UiState n
moveToTarget s = fromMaybe s $ do
  target <- uiTarget s
  let s' = s{uiFocused = ftPath target, uiTarget = Nothing}
  pure $ if ftChild target
    then moveFocusToFirstChild s'
    else moveFocusToNextSibling s'

-- | Try to find the closest parent to a 'Path' that exists in the 'Node'.
findValidParent :: Node -> Path -> Path
findValidParent _ (Path []) = Path []
findValidParent node (Path (x:xs)) = case applyId node x of
  Nothing    -> Path []
  Just child -> Path [x] <> findValidParent child (Path xs)

-- | Move to the closest valid parent as a last-ditch effort if the current
-- focus path becomes invalid.
moveToValidParent :: UiState n -> UiState n
moveToValidParent s =
  s{uiFocused = findValidParent (foldedRootNode s) (uiFocused s)}

-- | Modify the focused path so it always points to an existing node. Apply the
-- focus target if possible.
validateFocused :: UiState n -> UiState n
validateFocused = moveToValidParent . moveToTarget

-- | Close the editor if it doesn't point to a valid path.
validateEditor :: UiState n -> UiState n
validateEditor s = case uiEditor s of
  Nothing -> s
  Just e -> keepEditor $ fromMaybe False $ do
    node <- applyPath (uiRootNode s) (eiPath e)
    let flags = nodeFlags node
    pure $ if eiReply e then flagReply flags else flagEdit flags
  where
    keepEditor True  = s
    keepEditor False = s {uiEditor = Nothing}

-- | Modify the UI state so it is consistent again.
validate :: UiState n -> UiState n
validate = validateEditor . validateFocused . validateUnfolded

replaceRootNode :: Node -> UiState n -> UiState n
replaceRootNode node s = validate s
  { uiRootNode = node
  , uiFocused = findNextValidNode (uiRootNode s) node (uiFocused s)
  }

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

moveFocus :: (Node -> Path -> Path) -> UiState n -> UiState n
moveFocus f s = validateFocused s
  { uiFocused = f (foldedRootNode s) (uiFocused s)
  , uiTarget  = Nothing
  }

moveFocusUp :: UiState n -> UiState n
moveFocusUp = moveFocus findPrevNode

moveFocusDown :: UiState n -> UiState n
moveFocusDown = moveFocus findNextNode

moveFocusToParent :: UiState n -> UiState n
moveFocusToParent = moveFocus $ \_ focused -> fromMaybe focused $ parent focused

moveFocusToChild :: ([NodeId] -> Maybe NodeId) -> UiState n -> UiState n
moveFocusToChild f = moveFocus $ \node focused -> fromMaybe focused $ do
  siblings <- nodeChildren <$> applyPath node focused
  firstSiblingName <- f $ OMap.keys siblings
  pure $ focused <> Path [firstSiblingName]

moveFocusToFirstChild :: UiState n -> UiState n
moveFocusToFirstChild = moveFocusToChild headMay

moveFocusToLastChild :: UiState n -> UiState n
moveFocusToLastChild = moveFocusToChild lastMay

moveFocusToSibling :: ([NodeId] -> Maybe NodeId) -> UiState n -> UiState n
moveFocusToSibling f s
  | uiFocused s == mempty = s
  | otherwise = moveFocusToChild f $ moveFocusToParent s

moveFocusToFirstSibling :: UiState n -> UiState n
moveFocusToFirstSibling = moveFocusToSibling headMay

moveFocusToLastSibling :: UiState n -> UiState n
moveFocusToLastSibling = moveFocusToSibling lastMay

moveFocusToNextSibling :: UiState n -> UiState n
moveFocusToNextSibling s = fromMaybe s $ do
  (_, nodeId) <- splitInitLast $ uiFocused s
  pure $ moveFocusToSibling (findNext (==nodeId)) s

foldAtFocus :: UiState n -> UiState n
foldAtFocus s = validateUnfolded s
  { uiUnfolded = Set.delete (uiFocused s) (uiUnfolded s)
  , uiTarget   = Nothing
  }

unfoldAtFocus :: UiState n -> UiState n
unfoldAtFocus s = validateUnfolded s
  { uiUnfolded = Set.insert (uiFocused s) (uiUnfolded s)
  , uiTarget   = Nothing
  }

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
  in  validateEditor s{uiEditor = Just editorInfo, uiTarget = Nothing}

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

-- TODO use new functions from the node module
findTarget :: EditorInfo n -> UiState n -> FocusTarget
findTarget e s = fromMaybe (FocusTarget (eiPath e) (eiReply e)) $ do
  node <- applyPath (uiRootNode s) (eiPath e)
  lastChildId <- lastMay $ OMap.keys $ nodeChildren node
  let path = eiPath e <> Path [lastChildId]
  pure $ FocusTarget path False

finishEditing :: UiState n -> (UiState n, Maybe EditResult)
finishEditing s = fromMaybe (s, Nothing) $ do
  e <- uiEditor s
  let editResult = EditResult
        { erText  = getCurrentText $ eiEditor e
        , erPath  = eiPath e
        , erReply = eiReply e
        }
      s' = (abortEditing s){uiTarget = Just $ findTarget e s}
  pure (s', Just editResult)

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
  padRight Max $ txtWrap text
  where
    text
      | T.null $ nodeText node = " "
      | otherwise              = nodeText node

nodeToTree :: (Ord n, Show n) => UiState n -> Path -> Node -> Maybe [WidgetTree n] -> WidgetTree n
nodeToTree s path node maybeChildren = case uiEditor s of
  Just e | path == eiPath e ->
    let renderedEditor = renderNodeEditor $ eiEditor e
    in if eiReply e
       then WidgetTree renderedNode $ children ++ [WidgetTree renderedEditor []]
       else WidgetTree renderedEditor children
  _ -> WidgetTree renderedNode children
  where
    isFocused = path == uiFocused s
    renderedNode = renderNode isFocused node
    children = fromMaybe [] maybeChildren

renderUiState :: (Ord n, Show n) => IndentOptions -> UiState n -> Widget n
renderUiState opts s =
  renderWidgetTree opts $
  foldVisibleNodes (nodeToTree s) (uiUnfolded s) (uiRootNode s)
