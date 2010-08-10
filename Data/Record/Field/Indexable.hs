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
--
-- TODO: This should probably be a single-parameter type class with two
-- associated types instead.
class Indexable a i where
    type Element a :: *
    indexGet :: i -> a -> Maybe (Element a)
    indexSet :: i -> Maybe (Element a) -> a -> a
    unsafeIndexGet :: i -> a -> Element a
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
(#!) :: (Field a, Indexable (Dst a) i) =>
        a -> i -> Src a :-> Maybe (Element (Dst a))
f #! i = lens getter setter
    where getter a = indexGet i (getL (field f) a)
          setter v = modL (field f) (indexSet i v)

-- | As @(#!)@, but reading a nonexistent value will likely result in a
-- bottom value being returned. Also, the resulting field cannot be used
-- to remove values.
infixl 8 #!!
(#!!) :: (Field a, Indexable (Dst a) i) =>
         a -> i -> Src a :-> Element (Dst a)
f #!! i = lens getter setter
    where getter a = unsafeIndexGet i (getL (field f) a)
          setter v = setL (field $ f #! i) (Just v)

instance (Integral i) => Indexable [a] i where
    type Element [a] = a
    unsafeIndexGet i as = as !! fromIntegral i
    indexGet i as = case drop (fromIntegral i) as of
                            []    -> Nothing
                            (a:_) -> Just a
    indexSet i Nothing    as = before ++ drop 1 after
        where (before,after) = splitAt (fromIntegral i) as
    indexSet i (Just v)   as = before ++ (v : drop 1 after)
        where (before,after) = splitAt (fromIntegral i) as

instance (Ord k1, k1 ~ k2) => Indexable (Map.Map k1 a) k2 where
    type Element (Map.Map k1 a) = a
    unsafeIndexGet = flip (Map.!)
    indexGet = Map.lookup
    indexSet k v = Map.alter (const v) k

instance Indexable (IntMap.IntMap a) Int where
    type Element (IntMap.IntMap a) = a
    unsafeIndexGet = flip (IntMap.!)
    indexGet = IntMap.lookup
    indexSet k v = IntMap.alter (const v) k

instance (IArray.IArray a e, IArray.Ix i1, i1 ~ i2) =>
        Indexable (a i1 e) i2 where
    type Element (a i1 e) = e
    unsafeIndexGet = flip (IArray.!)
    indexGet i a
            | i >= min && i <= max = Just $ a IArray.! i
            | otherwise            = Nothing
        where (min, max) = IArray.bounds a

    indexSet i Nothing  a = a -- array elements can't be removed
    indexSet i (Just v) a
            | i >= min && i <= max = a IArray.// [(i,v)]
            | otherwise            = a
        where (min, max) = IArray.bounds a

instance (Ord a1, a1 ~ a2) => Indexable (Set.Set a1) a2 where
    type Element (Set.Set a1) = a1
    -- unsafeIndexGet doesn't really make sense here.
    indexGet a set | a `Set.member` set = Just a
                   | otherwise          = Nothing
    indexSet a Nothing  set = Set.delete a set
    indexSet a (Just _) set = Set.insert a set

instance Indexable IntSet.IntSet Int where
    type Element IntSet.IntSet = Int
    -- unsafeIndexGet doesn't really make sense here.
    indexGet a set | a `IntSet.member` set = Just a
                   | otherwise             = Nothing
    indexSet a Nothing  set = IntSet.delete a set
    indexSet a (Just _) set = IntSet.insert a set

