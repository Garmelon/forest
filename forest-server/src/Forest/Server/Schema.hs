module Forest.Server.Schema
  ( Schema
  , fork
  , fork'
  , leaf
  , collect
  , collectWith
  , dispatch
  ) where

import qualified Data.Text         as T

import           Forest.Node
import qualified Forest.OrderedMap as OMap

data Schema a
  = Fork T.Text (OMap.OrderedMap NodeId (Schema a))
  | Leaf a

instance Functor Schema where
  fmap f (Leaf a)             = Leaf $ f a
  fmap f (Fork text children) = Fork text $ fmap (fmap f) children

fork :: T.Text -> [(NodeId, Schema a)] -> Schema a
fork text = Fork text . OMap.fromList

fork' :: T.Text -> [Schema a] -> Schema a
fork' text = fork text . zip keys
  where
    keys :: [NodeId]
    keys = map (T.pack . show) [0::Int ..]

leaf :: a -> Schema a
leaf = Leaf

collect :: Schema Node -> Node
collect (Leaf node)          = node
collect (Fork text children) = Node text mempty $ OMap.map collect children

collectWith :: (a -> Node) -> Schema a -> Node
collectWith f = collect . fmap f

dispatch :: Path -> Schema a -> Maybe (Path, a)
dispatch path          (Leaf a)      = Just (path, a)
dispatch (Path (x:xs)) (Fork _ omap) = dispatch (Path xs) =<< (omap OMap.!? x)
dispatch (Path [])     (Fork _ _   ) = Nothing -- More specfic than required
