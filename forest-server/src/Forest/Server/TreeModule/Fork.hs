{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}

module Forest.Server.TreeModule.Fork
  ( ForkModule
  , ProngConstructor(..)
  , forkModule
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Trans.Cont
import qualified Data.Map                 as Map
import qualified Data.Text                as T

import           Forest.Node
import qualified Forest.OrderedMap        as OMap
import           Forest.Server.TreeModule

data Prong = forall r a . TreeModule a r => Prong (a r)

data ProngConstructor = forall r a . TreeModule a r =>
  ProngConstructor T.Text (ModuleConstructor (a r))

newtype ForkModule r = ForkModule (Map.Map NodeId Prong)

instance TreeModule ForkModule () where
  edit _ (Path []) _ = pure Nothing
  edit (ForkModule prongs) (Path (x:xs)) text = case prongs Map.!? x of
    Nothing -> pure Nothing
    Just (Prong a) -> do
      result <- edit a (Path xs) text
      pure $ () <$ result

  delete _ (Path []) = pure Nothing
  delete (ForkModule prongs) (Path (x:xs)) = case prongs Map.!? x of
    Nothing -> pure Nothing
    Just (Prong a) -> do
      result <- delete a (Path xs)
      pure $ () <$ result

  reply _ (Path []) _ = pure Nothing
  reply (ForkModule prongs) (Path (x:xs)) text = case prongs Map.!? x of
    Nothing -> pure Nothing
    Just (Prong a) -> do
      result <- reply a (Path xs) text
      pure $ () <$ result

  act _ (Path []) = pure Nothing
  act (ForkModule prongs) (Path (x:xs)) = case prongs Map.!? x of
    Nothing -> pure Nothing
    Just (Prong a) -> do
      result <- act a (Path xs)
      pure $ () <$ result

data ProngInfo = ProngInfo
  { piTopName :: T.Text
  , piNames   :: Map.Map NodeId T.Text
  , piNodes   :: Map.Map NodeId Node
  , piOrder   :: [NodeId]
  }

renderProngInfo :: ProngInfo -> Node
renderProngInfo pInfo =
  let childMap = Map.intersectionWith
        (\name node -> node{nodeText = name})
        (piNames pInfo)
        (piNodes pInfo)
      children = OMap.fromMapWithOrder childMap $ piOrder pInfo
  in  Node {nodeText = piTopName pInfo, nodeFlags = mempty, nodeChildren = children}

sendNodeFromProng :: MVar ProngInfo -> (Node -> IO ()) -> NodeId -> Node -> IO ()
sendNodeFromProng piVar sendNode nodeId node =
  modifyMVar_ piVar $ \pInfo -> do
    let newPInfo = pInfo {piNodes = Map.insert nodeId node $ piNodes pInfo}
    sendNode $ renderProngInfo newPInfo
    pure newPInfo

constructProngs
  :: MVar ProngInfo
  -> (Node -> IO ())
  -> Map.Map NodeId ProngConstructor
  -> Cont (IO ()) (Map.Map NodeId Prong)
constructProngs piVar sendNode =
  Map.traverseWithKey constructProng
  where
    constructProng nodeId (ProngConstructor _ constructor) =
      Prong <$> cont (constructor $ sendNodeFromProng piVar sendNode nodeId)

forkModule :: T.Text -> [ProngConstructor] -> ModuleConstructor (ForkModule ())
forkModule text prongs sendNode continue = do
  let namePairs = zip enumerateIds $ map (\(ProngConstructor name _) -> name) prongs
  nodesVar <- newMVar ProngInfo
    { piTopName = text
    , piNames = Map.fromList namePairs
    , piNodes = Map.empty
    , piOrder = map fst namePairs
    }
  let numbers = map (T.pack . show) [(0::Integer)..]
      prongMap = Map.fromList $ zip numbers prongs
  runCont (constructProngs nodesVar sendNode prongMap) (continue . ForkModule)
