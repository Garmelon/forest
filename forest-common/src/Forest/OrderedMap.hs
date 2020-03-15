module Forest.OrderedMap
  ( OrderedMap
  -- * Construction
  , empty
  , singleton
  , fromSet
  -- ** From lists
  , fromList
  , fromListWith
  , fromListWithKey
  -- ** From maps
  , fromMap
  , fromMapWithOrder
  -- * Insertion
  , append
  , appendWith
  , appendWithKey
  , appendLookupWithKey
  , prepend
  , prependWith
  , prependWithKey
  , prependLookupWithKey
  -- * Deletion/Update
  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  -- * Query
  -- ** Lookup
  , Forest.OrderedMap.lookup
  , (!?)
  , (!)
  , findWithDefault
  , member
  , notMember
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  -- ** Size
  , Forest.OrderedMap.null
  , size
  -- * Traversal
  -- ** Map
  , Forest.OrderedMap.map
  , mapWithKey
  -- * Conversion
  , elems
  , keys
  , assocs
  , keysSet
  -- ** Lists
  , toList
  -- ** Maps
  , toMap
  -- * Filter
  , Forest.OrderedMap.filter
  ) where

import qualified Data.List       as L
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

data OrderedMap k a = OrderedMap
  { omMap   :: Map.Map k a
  , omOrder :: [k]
  }

instance (Ord k, Show k, Show a) => Show (OrderedMap k a) where
  show m = "fromList " ++ show (toList m)

instance Functor (OrderedMap k) where
  fmap = Forest.OrderedMap.map

instance (Ord k) => Foldable (OrderedMap k) where
  foldMap f = foldMap f . elems

-- Invariants of this data type:
--
-- 1. The 'omOrder' list contains each key from 'omMap' exactly once.
-- 2. The 'omOrder' list contains no other values.
--
-- All functions operating on an 'OrderedMap' may assume these invariants. All
-- modifications must preserve these invariants.

-- | Like 'Map.empty'.
empty :: OrderedMap k a
empty = OrderedMap
  { omMap   = Map.empty
  , omOrder = []
  }

-- | Like 'Map.singleton'.
singleton :: k -> a -> OrderedMap k a
singleton k a = OrderedMap
  { omMap   = Map.singleton k a
  , omOrder = [k]
  }

-- | Like 'Map.fromSet'. Uses 'Set.toAscList' as the order.
fromSet :: (k -> a) -> Set.Set k -> OrderedMap k a
fromSet f kSet = OrderedMap
  { omMap   = Map.fromSet f kSet
  , omOrder = Set.toAscList kSet
  }

fromMap :: Map.Map k a -> OrderedMap k a
fromMap m = OrderedMap
  { omMap   = m
  , omOrder = Map.keys m
  }

-- | Create a new 'OrderedMap' from a 'Map.Map' using a list of keys to specify
-- the order. If the list of keys contains a key multiple times, only the last
-- occurrence is counted. The keys missing from the list of keys are appended at
-- the end in ascending order.
fromMapWithOrder :: Ord k => Map.Map k a -> [k] -> OrderedMap k a
fromMapWithOrder m l =
  let kSet = Map.keysSet m
      onlyExistingKeys = L.filter (`Set.member` kSet) l
      deduplicatedKeys = fst $ keepLastInstances onlyExistingKeys
      missingKeysSet = kSet Set.\\ Set.fromList deduplicatedKeys
      order = if Set.null missingKeysSet
        then deduplicatedKeys -- For the extra performance :P
        else deduplicatedKeys ++ Set.toAscList missingKeysSet
  in  OrderedMap {omMap = m, omOrder = order}

keepLastInstances :: Ord a => [a] -> ([a], Set.Set a)
keepLastInstances [] = ([], Set.empty)
keepLastInstances (x:xs)
  | x `Set.member` subset = (sublist, subset)
  | otherwise = (x : sublist, Set.insert x subset)
  where
    (sublist, subset) = keepLastInstances xs

-- | Like 'Map.fromList'. Uses the last occurrence of each key as the order.
fromList :: Ord k => [(k, a)] -> OrderedMap k a
-- This function could've been implemented as @fromListWith const@, but that
-- might lead to a performance penalty. I don't know though ¯\_(ツ)_/¯
fromList pairs = OrderedMap
  { omMap   = Map.fromList pairs
  , omOrder = fst $ keepLastInstances $ L.map fst pairs
  }

-- | Like 'Map.fromListWith'. Uses the last occurrence of each key as the order.
fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> OrderedMap k a
fromListWith f = fromListWithKey $ const f

-- | Like 'Map.fromListWithKey'. Uses the last occurrence of each key as the
-- order.
fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> OrderedMap k a
fromListWithKey f pairs = OrderedMap
  { omMap   = Map.fromListWithKey f pairs
  , omOrder = fst $ keepLastInstances $ L.map fst pairs
  }

append :: Ord k => k -> a -> OrderedMap k a -> OrderedMap k a
append = appendWith const

appendWith :: Ord k => (a -> a -> a) -> k -> a -> OrderedMap k a -> OrderedMap k a
appendWith f = appendWithKey $ const f

appendWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> OrderedMap k a -> OrderedMap k a
appendWithKey f k a = snd . appendLookupWithKey f k a

appendLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> OrderedMap k a -> (Maybe a, OrderedMap k a)
appendLookupWithKey f k a m =
  let (maybePrevA, newMap) = Map.insertLookupWithKey f k a $ omMap m
      newOrder = case maybePrevA of
        Nothing -> omOrder m ++ [k]
        Just _  -> omOrder m
  in  (maybePrevA, OrderedMap {omMap = newMap , omOrder = newOrder})

prepend :: Ord k => k -> a -> OrderedMap k a -> OrderedMap k a
prepend = prependWith const

prependWith :: Ord k => (a -> a -> a) -> k -> a -> OrderedMap k a -> OrderedMap k a
prependWith f = prependWithKey $ const f

prependWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> OrderedMap k a -> OrderedMap k a
prependWithKey f k a = snd . prependLookupWithKey f k a

prependLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> OrderedMap k a -> (Maybe a, OrderedMap k a)
prependLookupWithKey f k a m =
  let (maybePrevA, newMap) = Map.insertLookupWithKey f k a $ omMap m
      newOrder = case maybePrevA of
        Nothing -> k : omOrder m
        Just _  -> omOrder m
  in  (maybePrevA, OrderedMap {omMap = newMap , omOrder = newOrder})

delete :: Ord k => k -> OrderedMap k a -> OrderedMap k a
delete k m = m
  { omMap   = Map.delete k $ omMap m
  , omOrder = L.delete k $ omOrder m
  }

adjust :: Ord k => (a -> a) -> k -> OrderedMap k a -> OrderedMap k a
adjust f = adjustWithKey $ const f

adjustWithKey :: Ord k => (k -> a -> a) -> k -> OrderedMap k a -> OrderedMap k a
adjustWithKey f k m = m {omMap = Map.adjustWithKey f k $ omMap m}

update :: Ord k => (a -> Maybe a) -> k -> OrderedMap k a -> OrderedMap k a
update f = updateWithKey $ const f

updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> OrderedMap k a -> OrderedMap k a
updateWithKey f k m =
  let newMap = Map.updateWithKey f k $ omMap m
      newOrder = case newMap Map.!? k of
        Nothing -> L.delete k $ omOrder m
        Just _  -> omOrder m
  in  OrderedMap {omMap = newMap, omOrder = newOrder}

lookup :: Ord k => k -> OrderedMap k a -> Maybe a
lookup k = Map.lookup k . omMap

infixl 9 !?
(!?) :: Ord k => OrderedMap k a -> k -> Maybe a
m !? k = omMap m Map.!? k

infixl 9 !
(!) :: Ord k => OrderedMap k a -> k -> a
m ! k = omMap m Map.! k

findWithDefault :: Ord k => a -> k -> OrderedMap k a -> a
findWithDefault a k = Map.findWithDefault a k . omMap

member :: Ord k => k -> OrderedMap k a -> Bool
member k = Map.member k . omMap

notMember :: Ord k => k -> OrderedMap k a -> Bool
notMember k = Map.notMember k . omMap

lookupLT :: Ord k => k -> OrderedMap k v -> Maybe (k, v)
lookupLT k = Map.lookupLT k . omMap

lookupGT :: Ord k => k -> OrderedMap k v -> Maybe (k, v)
lookupGT k = Map.lookupGT k . omMap

lookupLE :: Ord k => k -> OrderedMap k v -> Maybe (k, v)
lookupLE k = Map.lookupLE k . omMap

lookupGE :: Ord k => k -> OrderedMap k v -> Maybe (k, v)
lookupGE k = Map.lookupGE k . omMap

null :: OrderedMap k a -> Bool
null = Map.null . omMap

size :: OrderedMap k a -> Int
size = Map.size . omMap

map :: (a -> b) -> OrderedMap k a -> OrderedMap k b
map f = mapWithKey $ const f

mapWithKey :: (k -> a -> b) -> OrderedMap k a -> OrderedMap k b
mapWithKey f m = m {omMap = Map.mapWithKey f $ omMap m}

elems :: Ord k => OrderedMap k a -> [a]
elems = L.map snd . assocs

keys :: OrderedMap k a -> [k]
keys = omOrder

assocs :: Ord k => OrderedMap k a -> [(k, a)]
assocs = toList

keysSet :: OrderedMap k a -> Set.Set k
keysSet = Map.keysSet . omMap

toList :: Ord k => OrderedMap k a -> [(k, a)]
toList m = L.map (\k -> (k, omMap m Map.! k)) $ omOrder m

toMap :: OrderedMap k a -> Map.Map k a
toMap = omMap

filter :: Ord k => (a -> Bool) -> OrderedMap k a -> OrderedMap k a
filter f m =
  let newMap = Map.filter f $ omMap m
      newOrder = L.filter (`Set.member` Map.keysSet newMap) $ omOrder m
  in  OrderedMap {omMap = newMap, omOrder = newOrder}
