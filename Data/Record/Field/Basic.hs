{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | The @'Field'@ class and basic operations.

module Data.Record.Field.Basic
    ( -- * Record fields.
      Field(..)
      -- * Basic field operators.
    , (.#)
    , (=:)
    , (=~)
      -- * Pattern matching.
    , match
    ) where

import Data.Record.Label hiding ((=:))

-- | Instances of this class can be combined with the functions and
-- operators in this package.
class Field a where
    -- | The source type of the field. I.e. the record type.
    type Src a :: *
    -- | The destination type of the field. I.e. the type of the field
    -- in question.
    type Dst a :: *
    -- | Return an @"fclabels"@ lens corresponding to this field.
    field :: a -> (Src a :-> Dst a)

instance Field (a :-> b) where
    type Src (a :-> b) = a
    type Dst (a :-> b) = b
    field              = id

infixl 7 .#
-- | Return the value of the field in the given record.
(.#) :: (Field a) => Src a -> a -> Dst a
r .# f = getL (field f) r

-- | Infix assignment lookalike.
--
-- > r.#f =: v
--
-- returns a modified version of @r@ so that the field corresponding to
-- @f@ are set to @v@.
infixl 8 =:
(=:) :: (Field a) => a -> Dst a -> Src a :-> Src a
a =: v = lens (setL (field a) v) const

-- | Infix modification lookalike.
--
-- > r.#f =~ g
--
-- returns a modified version of @r@ so that the fields corresponding to
-- @f@ are modified with the function @g@.
infixl 8 =~
(=~) :: (Field a) => a -> (Dst a -> Dst a) -> Src a :-> Src a
a =~ f = lens (modL (field a) f) const

-- | Convenience function for use with the @ViewPatterns@ extension.
--
-- > case r of
-- >      (match int -> 5)                   -> "It's 5!"
-- >      (match (int,str#$length) -> (i,l))
-- >            | i == l                     -> "They're equal!"
-- >            | otherwise                  -> "Not equal."
-- >      _                                  -> "Something else."
--
match :: (Field a) => a -> Src a -> Dst a
match f = (.# f)

