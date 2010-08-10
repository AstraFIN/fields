{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Field combinators.
module Data.Record.Field.Combinators
    ( -- * Basic combinators.
      idL
    , ( # )
    , (#$ )
      -- * Combinators for @'Functor'@s, @'Applicative'@s and
      -- @'Monad'@s.
    , (<.#>)
    , (<#>)
    , (<##>)
      -- * Zippy assignment.
    , (*#)
    , (=*)
      -- * Assignment and modification in a State monad.
    , (<=:)
    , (<=~)
      -- * Utility combinator for comparisons etc.
    , onField
    ) where

import Data.Record.Field.Basic
import Data.Record.Label hiding ((=:))
import qualified Control.Category as C
import Control.Applicative
import Control.Monad
import "monads-fd" Control.Monad.State.Class

cantSet :: String -> a
cantSet n = error $ n ++ ": cannot set values."

-- | Identity lens.
idL :: a :-> a
idL = C.id

-- | Field composition with arguments in OO-like order.
infixl 8 #
( # ) :: (Field a, Field b, Dst a ~ Src b) =>
         a -> b -> Src a :-> Dst b
a # b = (field b) C.. (field a)

-- | Compose fields with ordinary functions. As functions are one-way,
-- the resulting field cannot be used to set values.
infixl 8 #$
(#$) :: (Field a) => a -> (Dst a -> b) -> Src a :-> b
ab #$ f = lens getter (cantSet "(#$)")
    where getter a = f . getL (field ab) $ a

-- | Infix @'fmap'@ for fields.
--
-- Examples:
--
-- 
-- > persons <.#> firstName
--
-- > do (v1, v2) <- takeMVar mv <.#> (field1, field2)
-- >    putStrLn . unlines $ [ "v1: " ++ show v1, "v2: " ++ show v2 ]
--
infixl 7 <.#>
(<.#>) :: (Functor f, Field a) => f (Src a) -> a -> f (Dst a)
f <.#> a = fmap (.# a) f

-- | @'Applicative'@ functor composition for fields.
--
-- > book .# characters <#> lastName
--
infixr 9 <#>
(<#>) :: (Applicative f, Field a, Field b, Dst a ~ f (Src b)) =>
          a -> b -> Src a :-> f (Dst b)
ab <#> bc = lens getter setter
    where getter    = (fmap $ getL (field bc)) . getL (field ab)
          -- the flip is so effects get performed for b first.
          setter fc =  modL (field ab) $
                \fb -> flip (setL (field bc)) <$> fb <*> fc

-- | Flattening monadic composition for fields.
--
-- > person .# superior <##> superior <##> superior <##> superior
--
infixr 9 <##>
(<##>) :: (Monad m, Field a, Field b,
           Dst a ~ m (Src b), Dst b ~ m c) =>
           a -> b -> Src a :-> m c
ab <##> bc = lens getter setter
    where getter    = getL (field ab) >=> getL (field bc)
          setter mc = modL (field ab) $ 
                \mb -> do b <- mb
                          return $ setL (field bc) mc b

-- | Zippy field reference to be used with @('=*')@.
--
-- > [ rec1, rec2 ] *# field =* [ value1, value2 ]
--
infixl 7 *#
(*#) :: (Field b) => [Src b] -> [b] -> [Dst b]
rs *# as = zipWith (.#) rs as 

-- | Zippy infix assignment to be used with @('*#')@.
infixl 8 =*
(=*) :: (Field a) => a -> [Dst a] -> [Src a :-> Src a]
a =* vs = [ a =: v | v <- vs ]

-- | Infix assignment for the State monad.
--
-- > (field1, field2) <=: (value1, value2)
--
infix 3 <=:
(<=:) :: (MonadState (Src a) m, Field a) =>
         a -> Dst a -> m ()
a <=: v = modify (.# a =: v)

-- | Infix modification for the State monad.
--
-- > (field1, field2) <=~ (f, g)
--
infix 3 <=~
(<=~) :: (MonadState (Src a) m, Field a) =>
         a -> (Dst a -> Dst a) -> m ()
a <=~ f = modify (.# a =~ f)

-- | Utility combinator in the manner of @'Data.Function.on'@.
--
-- > sortBy (compare `onField` (lastName,firstName)) persons
--
infixl 0 `onField`
onField :: (Field a) => (Dst a -> Dst a -> t) -> a -> Src a -> Src a -> t
onField f a r1 r2 = f (r1.#a) (r2.#a)

