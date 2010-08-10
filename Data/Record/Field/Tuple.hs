-- vim: encoding=latin1
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | Instances for tuples of fields up to a 10-tuple. This allows
-- accessing several fields simultaneously.
--
-- > r.#(field1, field2, field3#field4) =: (value1, value2, value3)
--
-- In addition, the pair instance is recursively defined, which allows
-- stuff like
--
-- > import Control.Arrow ((***))
-- > r.#(field1, (field2, field3)) =~ (f *** g *** h)
--
module Data.Record.Field.Tuple
    ( 
    ) where

import Data.Record.Field.Basic
import Data.Record.Field.Combinators
import Data.Record.Label hiding ((=:))
{- Commented out to remove the dependency to pretty.
import Text.PrettyPrint hiding (int)
import qualified Text.PrettyPrint as PP
-}


instance (Field f, r ~ Src f) => Field (r :-> a, f) where
    type Src (r :-> a, f) = r
    type Dst (r :-> a, f) = (a, Dst f)
    field (l1, f) = lens get set
        where get r      = (getL l1 r,  getL l2 r)
              set (a, b) =  setL l2 b . setL l1 a
              l2         = field f

{- Commented out to remove the dependency to pretty.
mkTupleFieldInstance :: Int -> String
mkTupleFieldInstance n = render inst
    where inst = header $+$ nest 4 defs
          header =   text "instance Field" <+> typ <+> text "where"

          typ  = tupleOf [ text "r :->" <+> v | v <- vs ]
          vals = tupleOf vs

          defs = vcat [rec, val, field, accs]

          tupleOf = parens . commaSep
          commaSep = sep . punctuate (text ",")

          rs = [ text "r" <> PP.int i | i <- [1..n] ]
          vs = take n $ [ text [v]      | v  <- ['a'..'z'] ] ++
                        [ text [v1,v2]  | v1 <- ['a'..'z']
                                        , v2 <- ['a'..'z'] ]

          rec = text "type Src" <+> typ <+> text "= r"
          val = text "type Dst" <+> typ <+> text "=" <+> vals
          field = text "field" <+> tupleOf rs <+> text "= lens get set"
          accs = nest 4 $ text "where" <+> vcat [getter, setter]

          getter = text "get r =" <+> tupleOf [ get r | r <- rs ]
          setter = text "set" <+> vals <+> text "=" <+>
              (sep . punctuate (text " .")) [ set r v | (r,v) <- zip rs vs ]

          get r   = text "getL" <+> r <+> text "r"
          set r v = text "setL" <+> r <+> v
-}

instance Field (r :-> a, r :-> b, r :-> c) where
    type Src (r :-> a, r :-> b, r :-> c) = r
    type Dst (r :-> a, r :-> b, r :-> c) = (a, b, c)
    field (r1, r2, r3) = lens get set
        where get r = (getL r1 r, getL r2 r, getL r3 r)
              set (a, b, c) = setL r1 a . setL r2 b . setL r3 c
instance Field (r :-> a, r :-> b, r :-> c, r :-> d) where
    type Src (r :-> a, r :-> b, r :-> c, r :-> d) = r
    type Dst (r :-> a, r :-> b, r :-> c, r :-> d) = (a, b, c, d)
    field (r1, r2, r3, r4) = lens get set
        where get r = (getL r1 r, getL r2 r, getL r3 r, getL r4 r)
              set (a, b, c, d) = setL r1 a . setL r2 b . setL r3 c . setL r4 d
instance Field (r :-> a, r :-> b, r :-> c, r :-> d, r :-> e) where
    type Src (r :-> a, r :-> b, r :-> c, r :-> d, r :-> e) = r
    type Dst (r :-> a, r :-> b, r :-> c, r :-> d, r :-> e) = (a,
                                                              b,
                                                              c,
                                                              d,
                                                              e)
    field (r1, r2, r3, r4, r5) = lens get set
        where get r = (getL r1 r,
                       getL r2 r,
                       getL r3 r,
                       getL r4 r,
                       getL r5 r)
              set (a, b, c, d, e) = setL r1 a .
                                    setL r2 b .
                                    setL r3 c .
                                    setL r4 d .
                                    setL r5 e
instance Field (r :-> a,
                r :-> b,
                r :-> c,
                r :-> d,
                r :-> e,
                r :-> f) where
    type Src (r :-> a, r :-> b, r :-> c, r :-> d, r :-> e, r :-> f) = r
    type Dst (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f) = (a, b, c, d, e, f)
    field (r1, r2, r3, r4, r5, r6) = lens get set
        where get r = (getL r1 r,
                       getL r2 r,
                       getL r3 r,
                       getL r4 r,
                       getL r5 r,
                       getL r6 r)
              set (a, b, c, d, e, f) = setL r1 a .
                                       setL r2 b .
                                       setL r3 c .
                                       setL r4 d .
                                       setL r5 e .
                                       setL r6 f
instance Field (r :-> a,
                r :-> b,
                r :-> c,
                r :-> d,
                r :-> e,
                r :-> f,
                r :-> g) where
    type Src (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f,
              r :-> g) = r
    type Dst (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f,
              r :-> g) = (a, b, c, d, e, f, g)
    field (r1, r2, r3, r4, r5, r6, r7) = lens get set
        where get r = (getL r1 r,
                       getL r2 r,
                       getL r3 r,
                       getL r4 r,
                       getL r5 r,
                       getL r6 r,
                       getL r7 r)
              set (a, b, c, d, e, f, g) = setL r1 a .
                                          setL r2 b .
                                          setL r3 c .
                                          setL r4 d .
                                          setL r5 e .
                                          setL r6 f .
                                          setL r7 g
instance Field (r :-> a,
                r :-> b,
                r :-> c,
                r :-> d,
                r :-> e,
                r :-> f,
                r :-> g,
                r :-> h) where
    type Src (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f,
              r :-> g,
              r :-> h) = r
    type Dst (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f,
              r :-> g,
              r :-> h) = (a, b, c, d, e, f, g, h)
    field (r1, r2, r3, r4, r5, r6, r7, r8) = lens get set
        where get r = (getL r1 r,
                       getL r2 r,
                       getL r3 r,
                       getL r4 r,
                       getL r5 r,
                       getL r6 r,
                       getL r7 r,
                       getL r8 r)
              set (a, b, c, d, e, f, g, h) = setL r1 a .
                                             setL r2 b .
                                             setL r3 c .
                                             setL r4 d .
                                             setL r5 e .
                                             setL r6 f .
                                             setL r7 g .
                                             setL r8 h
instance Field (r :-> a,
                r :-> b,
                r :-> c,
                r :-> d,
                r :-> e,
                r :-> f,
                r :-> g,
                r :-> h,
                r :-> i) where
    type Src (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f,
              r :-> g,
              r :-> h,
              r :-> i) = r
    type Dst (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f,
              r :-> g,
              r :-> h,
              r :-> i) = (a, b, c, d, e, f, g, h, i)
    field (r1, r2, r3, r4, r5, r6, r7, r8, r9) = lens get set
        where get r = (getL r1 r,
                       getL r2 r,
                       getL r3 r,
                       getL r4 r,
                       getL r5 r,
                       getL r6 r,
                       getL r7 r,
                       getL r8 r,
                       getL r9 r)
              set (a, b, c, d, e, f, g, h, i) = setL r1 a .
                                                setL r2 b .
                                                setL r3 c .
                                                setL r4 d .
                                                setL r5 e .
                                                setL r6 f .
                                                setL r7 g .
                                                setL r8 h .
                                                setL r9 i
instance Field (r :-> a,
                r :-> b,
                r :-> c,
                r :-> d,
                r :-> e,
                r :-> f,
                r :-> g,
                r :-> h,
                r :-> i,
                r :-> j) where
    type Src (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f,
              r :-> g,
              r :-> h,
              r :-> i,
              r :-> j) = r
    type Dst (r :-> a,
              r :-> b,
              r :-> c,
              r :-> d,
              r :-> e,
              r :-> f,
              r :-> g,
              r :-> h,
              r :-> i,
              r :-> j) = (a, b, c, d, e, f, g, h, i, j)
    field (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10) = lens get set
        where get r = (getL r1 r,
                       getL r2 r,
                       getL r3 r,
                       getL r4 r,
                       getL r5 r,
                       getL r6 r,
                       getL r7 r,
                       getL r8 r,
                       getL r9 r,
                       getL r10 r)
              set (a, b, c, d, e, f, g, h, i, j) = setL r1 a .
                                                   setL r2 b .
                                                   setL r3 c .
                                                   setL r4 d .
                                                   setL r5 e .
                                                   setL r6 f .
                                                   setL r7 g .
                                                   setL r8 h .
                                                   setL r9 i .
                                                   setL r10 j

