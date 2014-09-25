{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.HashMap.NonEmpty
    ( NonEmpty
    , fromList
    , fromListWith
    , fromNonEmptyList
    , fromNonEmptyListWith
    , singleton
    , union
    , unionWith
    , insert
    , insertWith
    , member
    , foldrWithKey
    , lookup
    , toList
    , toNonEmptyList
    , toHashMap
    ) where

import Data.Foldable ( Foldable, foldMap )
import Data.Hashable ( Hashable(..) )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import Data.Monoid ( (<>) )

import Prelude hiding ( lookup, init )

data NonEmpty k v where
    NEHM :: (Hashable k, Eq k)
         => (k, v)        -- ^ The key/val witnessing that the map isn't empty.
         -> HashMap k v   -- ^ The other elements (perhaps an empty map).
         -> NonEmpty k v

-- Use HashMap equality so that the NEHMs with the same elements inserted
-- in a different order are treated as equal.
instance (Eq k, Eq v) => Eq (NonEmpty k v) where
    (NEHM (ak, av) am) == (NEHM (bk, bv) bm) =
        HM.insert ak av am == HM.insert bk bv bm

instance Functor (NonEmpty k) where
        fmap f (NEHM (k, v) m) = NEHM (k, f v) (HM.map f m)

deriving instance (Show k, Show v) => Show (NonEmpty k v)

instance Foldable (NonEmpty k) where
    foldMap f (NEHM (_, v) hm) = f v <> foldMap f hm

-- |Create a NEHM containing a single key/value entry.
singleton :: (Hashable k, Eq k) => k -> v -> NonEmpty k v
singleton k v = NEHM (k, v) HM.empty

-- |Attempts to construct a NEHM from a list of key/value pairs, returning
-- Nothing in the case of an empty list.
fromList :: (Hashable k, Eq k) => [(k, v)] -> Maybe (NonEmpty k v)
fromList = fromListWith const

fromListWith :: (Hashable k, Eq k) => (v -> v -> v) -> [(k, v)]
             -> Maybe (NonEmpty k v)
fromListWith _ [] = Nothing
fromListWith f (kv : kvs) = Just $ fromNonEmptyListWith f (kv NEL.:| kvs)

-- |Convert a NonEmpty list into a NEHM.
fromNonEmptyList :: (Hashable k, Eq k) => NEL.NonEmpty (k, v) -> NonEmpty k v
fromNonEmptyList = fromNonEmptyListWith const

-- |Convert a NonEmpty list into a NEHM, combining duplicate keys with the
-- passed function.
fromNonEmptyListWith :: (Hashable k, Eq k) => (v -> v -> v) -> NEL.NonEmpty (k, v) -> NonEmpty k v
fromNonEmptyListWith f ((k1, v1) NEL.:| kvs) = NEHM (k1, v) hm
  where
    kvHM = HM.fromList kvs
    (v, hm) = case HM.lookup k1 kvHM of
                  Nothing -> (v, kvHM)
                  Just v1' -> (f v1 v1', HM.delete k1 kvHM)

-- |Unions two NEHMs together, preferring entries from the first map for
-- duplicate keys.
union :: NonEmpty k v -> NonEmpty k v -> NonEmpty k v
union = unionWith const


-- |Unions two NEHMs together, using the provided combination function in
-- the case of duplicate keys.
unionWith :: (v -> v -> v) -> NonEmpty k v -> NonEmpty k v -> NonEmpty k v
unionWith f (NEHM (k1, v1) hm1) (NEHM (k2, v2) hm2) =
    NEHM (k1, v) hm
  where
    hmBoth = hm1 `HM.union` hm2

    -- Only insert k1 into the hashset if it won't otherwise be in the NEHM.
    (v, hm) = if k1 == k2
                  then (f v1 v2, hmBoth)
                  else (v1, HM.insert k2 v2 hmBoth)

-- Insert a new mapping, using the existing value if the key is already
-- present in the NEHM.
insert :: k -> v -> NonEmpty k v -> NonEmpty k v
insert = insertWith const

-- Insert a new mapping, using the provided function in the case of
-- an already-present key.
insertWith :: (v -> v -> v) -> k -> v -> NonEmpty k v -> NonEmpty k v
insertWith f k' v' (NEHM (k, v) hm) =
    if k' == k
        then NEHM (k, f v v') hm
        else NEHM (k, v) (HM.insertWith f k' v' hm)

-- |Checks if an key is a member of a NEHM.
member :: k -> NonEmpty k v -> Bool
member needle (NEHM (k, _) hm) = needle == k || HM.member k hm

-- |Fold over a NEHM
foldrWithKey :: (k -> v -> r -> r) -> r -> NonEmpty k v -> r
foldrWithKey f init nehm = HM.foldrWithKey f init $ toHashMap nehm

lookup :: k -> NonEmpty k v -> Maybe v
lookup needle (NEHM (k, v) hm)
    | needle == k = Just v
    | otherwise = HM.lookup needle hm
-- |Convert a NEHM into a list of key/value pairs.
toList :: NonEmpty k v -> [(k, v)]
toList (NEHM kv hm) = kv : HM.toList hm

-- |Convert a NES into a NonEmpty (list) of its elements.
toNonEmptyList :: NonEmpty k v -> NEL.NonEmpty (k, v)
toNonEmptyList (NEHM (k, v) hm) = (k, v) NEL.:| HM.toList hm

-- |Convert a NEHM into a HashMap
toHashMap :: NonEmpty k v -> HashMap k v
toHashMap (NEHM (k, v) hm) = HM.insert k v hm
