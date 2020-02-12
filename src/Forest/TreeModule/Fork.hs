{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}

module Forest.TreeModule.Fork
  ( ForkModule
  , ProngConstructor(..)
  , forkModule
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Trans.Cont
import qualified Data.Map                 as Map
import qualified Data.Text                as T

import           Forest.Node
import           Forest.TreeModule

data Prong = forall a . TreeModule a => Prong a
data ProngConstructor = forall a . TreeModule a => ProngConstructor (ModuleConstructor a)

newtype ForkModule = ForkModule (Map.Map NodeId Prong)

instance TreeModule ForkModule where
  edit _ (Path []) _ = pure ()
  edit (ForkModule prongs) (Path (x:xs)) text = case prongs Map.!? x of
    Just (Prong a) -> edit a (Path xs) text
    Nothing -> pure ()

  delete _ (Path []) = pure ()
  delete (ForkModule prongs) (Path (x:xs)) = case prongs Map.!? x of
    Just (Prong a) -> delete a (Path xs)
    Nothing -> pure ()

  reply _ (Path []) _ = pure ()
  reply (ForkModule prongs) (Path (x:xs)) text = case prongs Map.!? x of
    Just (Prong a) -> reply a (Path xs) text
    Nothing -> pure ()

  act _ (Path []) = pure ()
  act (ForkModule prongs) (Path (x:xs)) = case prongs Map.!? x of
    Just (Prong a) -> act a (Path xs)
    Nothing -> pure ()

sendNodeFromProng
  :: T.Text
  -> MVar (Map.Map NodeId Node)
  -> (Node -> IO ())
  -> NodeId
  -> Node
  -> IO ()
sendNodeFromProng text nodesVar sendNode nodeId node = do
  nodes <- takeMVar nodesVar
  let newNodes = Map.insert nodeId node nodes
      newTopNode = Node text False False False False newNodes
  sendNode newTopNode
  putMVar nodesVar newNodes

constructProngs
  :: T.Text
  -> MVar (Map.Map NodeId Node)
  -> (Node -> IO ())
  -> Map.Map NodeId ProngConstructor
  -> Cont (IO ()) (Map.Map NodeId Prong)
constructProngs text nodesVar sendNode =
  Map.traverseWithKey constructProng
  where
    constructProng nodeId (ProngConstructor constructor) =
      Prong <$> cont (constructor $ sendNodeFromProng text nodesVar sendNode nodeId)

forkModule :: T.Text -> [ProngConstructor] -> ModuleConstructor ForkModule
forkModule text prongs sendNode continue = do
  nodesVar <- newMVar Map.empty
  let digits = length $ show $ length prongs
      numbers = map (T.justifyRight digits '0' . T.pack . show) [(0::Integer)..]
      prongMap = Map.fromList $ zip numbers prongs
  runCont (constructProngs text nodesVar sendNode prongMap) (continue . ForkModule)
