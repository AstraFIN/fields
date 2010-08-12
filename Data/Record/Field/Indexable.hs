-- vim: encoding=latin1
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | Composition operators for collection fields.
module Data.Record.Field.Indexable
    ( Indexable(..)
    , (#!)
    , (#!!)
    ) where

import Data.Record.Field.Basic
import Data.Record.Label
import qualified Data.Map          as Map
import qualified Data.IntMap       as IntMap
import qualified Data.Array.IArray as IArray
import qualified Data.Set          as Set
import qualified Data.IntSet       as IntSet

-- | Class of collection types that can be indexed into.
class Indexable a where
    type IndexOf   a :: *
    type ElementOf a :: *
    indexGet :: IndexOf a -> a -> Maybe (ElementOf a)
    indexSet :: IndexOf a -> Maybe (ElementOf a) -> a -> a
    unsafeIndexGet :: IndexOf a -> a -> ElementOf a
    unsafeIndexGet i a = maybe notFound id $ indexGet i a
        where notFound = error "unsafeIndexGet: element not found"

-- | Compose a field with an @'Indexable'@ collection safely. 
--
-- > r .# coll #! idx
--
-- returns @Nothing@ if @idx@ was not found from the collection, and
-- @Just v@ if @v@ was found.
--
-- > r .# coll #! idx =: Just v
--
-- sets the value at @idx@ in the collection to be @v@. If the value
-- wasn't in the collection, it's inserted. The exact semantics of
-- insertion depend on the actual collection in question.
--
-- > r .# coll #! idx =: Nothing
--
-- removes the value at @idx@ from the collection, if possible.
--
infixl 8 #!
(#!) :: (Field a, Indexable (Dst a)) =>
        a -> IndexOf (Dst a) -> Src a :-> Maybe (ElementOf (Dst a))
f #! i = lens getter setter
    where getter a = indexGet i (getL (field f) a)
          setter v = modL (field f) (indexSet i v)

-- | As @(#!)@, but reading a nonexistent value will likely result in a
-- bottom value being returned. Also, the resulting field cannot be used
-- to remove values.
infixl 8 #!!
(#!!) :: (Field a, Indexable (Dst a)) =>
         a -> IndexOf (Dst a) -> Src a :-> ElementOf (Dst a)
f #!! i = lens getter setter
    where getter a = unsafeIndexGet i (getL (field f) a)
          setter v = setL (field $ f #! i) (Just v)

instance Indexable [a] where
    type IndexOf   [a] = Int
    type ElementOf [a] = a
    unsafeIndexGet i as = as !! i
    indexGet i as = case drop i as of
                            []    -> Nothing
                            (a:_) -> Just a
    indexSet i Nothing    as = before ++ drop 1 after
        where (before,after) = splitAt i as
    indexSet i (Just v)   as = before ++ (v : drop 1 after)
        where (before,after) = splitAt i as

instance (Ord k) => Indexable (Map.Map k a) where
    type IndexOf   (Map.Map k a) = k
    type ElementOf (Map.Map k a) = a
    unsafeIndexGet = flip (Map.!)
    indexGet = Map.lookup
    indexSet k v = Map.alter (const v) k

instance Indexable (IntMap.IntMap a) where
    type IndexOf   (IntMap.IntMap a) = Int
    type ElementOf (IntMap.IntMap a) = a
    unsafeIndexGet = flip (IntMap.!)
    indexGet = IntMap.lookup
    indexSet k v = IntMap.alter (const v) k

instance (Ord a) => Indexable (Set.Set a) where
    type IndexOf   (Set.Set a) = a
    type ElementOf (Set.Set a) = a
    -- unsafeIndexGet doesn't really make sense here.
    indexGet a set | a `Set.member` set = Just a
                   | otherwise          = Nothing
    indexSet a Nothing  set = Set.delete a set
    indexSet a (Just _) set = Set.insert a set

instance Indexable IntSet.IntSet where
    type   IndexOf IntSet.IntSet = Int
    type ElementOf IntSet.IntSet = Int
    -- unsafeIndexGet doesn't really make sense here.
    indexGet a set | a `IntSet.member` set = Just a
                   | otherwise             = Nothing
    indexSet a Nothing  set = IntSet.delete a set
    indexSet a (Just _) set = IntSet.insert a set

