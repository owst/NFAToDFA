{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.HashSet.NonEmpty
    ( NonEmpty
    , fromList
    , fromNonEmptyList
    , map
    , singleton
    , insert
    , union
    , member
    , toList
    , toNonEmptyList
    , toHashSet
    ) where

import Data.Foldable ( Foldable, foldMap )
import Data.Hashable ( Hashable(..) )
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import Data.Monoid ( (<>) )

import Prelude hiding ( elem, map )

data NonEmpty a where
    NES :: (Hashable a, Eq a)
        => a         -- ^ The element witnessing that the set isn't empty.
        -> HashSet a -- ^ The other elements (perhaps an empty set).
        -> NonEmpty a

-- Use HashSet equality so that the NESs with the same elements inserted in a
-- different order are treated as equal.
instance (Eq a) => Eq (NonEmpty a) where
    (NES a as) == (NES b bs) = HS.insert a as == HS.insert b bs

instance (Hashable a) => Hashable (NonEmpty a) where
    -- sort, to have equal hashes between (NES 1 [2]) and (NES 2 [1])
    hashWithSalt salt nes =
        salt `hashWithSalt` (L.sort . L.map hash $ toList nes)

deriving instance (Show a) => Show (NonEmpty a)

instance Foldable NonEmpty where
    foldMap f (NES e hs) = f e <> foldMap f hs

-- |Create a NES containing a single element.
singleton :: (Hashable a, Eq a) => a -> NonEmpty a
singleton x = NES x HS.empty

-- |Attempts to construct a NES from a list of elements, returning Nothing in
-- the case of an empty list.
fromList :: (Hashable a, Eq a) => [a] -> Maybe (NonEmpty a)
fromList [] = Nothing
fromList xs@(x : _) =
    -- Make sure that if x appears more than once in the input list, it appears
    -- only once in the NES.
    Just $ NES x (HS.delete x $ HS.fromList xs)

-- |Convert a NonEmpty list into a NES.
fromNonEmptyList :: (Hashable a, Eq a) => NEL.NonEmpty a -> NonEmpty a
fromNonEmptyList (x NEL.:| xs) = NES x (HS.delete x $ HS.fromList xs)

-- |Convert the elements of the NonEmpty using a function @f@. @f@ should be
-- injective, to preserve invaraints.
map :: (Eq b, Hashable b) => (a -> b) -> NonEmpty a -> NonEmpty b
map f (NES a hs) = NES (f a) (HS.map f hs)

-- |Inserts a new element into the NES. We must be careful, to avoid adding the
-- chosen initial element into the other elements.
insert :: a -> NonEmpty a -> NonEmpty a
insert a nes@(NES e s) = if a == e then nes else NES e (HS.insert a s)

-- |Unions two NESs together.
union :: NonEmpty a -> NonEmpty a -> NonEmpty a
union (NES e1 s1) (NES e2 s2) = NES e1 s
  where
    sBoth = s1 `HS.union` s2

    -- Only insert e2 into the hashset if it won't otherwise be in the NES.
    s = if e1 == e2
            then sBoth
            else e2 `HS.insert` sBoth

-- |Checks if an element is a member of a NES.
member :: a -> NonEmpty a -> Bool
member e (NES a hs) = e == a || HS.member e hs

-- |Convert a NES into a canonical list of elements, "forgetting" the
-- chosen element by first converting to a HashSet and then to a list.
toList :: NonEmpty a -> [a]
toList = HS.toList . toHashSet

-- |Convert a NES into a canonical NonEmpty (list) of its elements.
toNonEmptyList :: NonEmpty a -> NEL.NonEmpty a
toNonEmptyList nes = case toList nes of
                         [] -> error "impossible!"
                         a : as -> a NEL.:| as

-- |Convert a NES into a HashSet.
toHashSet :: NonEmpty a -> HashSet a
toHashSet (NES a hs) = a `HS.insert` hs
