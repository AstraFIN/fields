{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where 

import Data.List
import Control.Applicative
import Control.Arrow ((***))
import "monads-fd" Control.Monad.State

import Data.Record.Field
import Data.Record.Label hiding ((=:))

data Person = Person
     { _firstName :: String
     , _lastName  :: String
     , _age       :: Int
     , _superior  :: Maybe Person
     }

data Book = Book
     { _title      :: String
     , _author     :: Person
     , _characters :: [Person]
     }

$(mkLabels [''Person, ''Book])

parens s = concat ["(", s, ")"]

-- We could just derive Show, but let's try to eat our own dog food
-- here.
instance Show Person where
    -- Arguably, RecordWildCards would be nicer here, but nothing
    -- prevents you from using it along with this package.
    show (match (firstName, lastName, age, superior) ->
                (f, l, a, s)) = parens . unwords $ [ "Person"
                                                   , show f
                                                   , show l
                                                   , show a
                                                   , show s ]

instance Show Book where
    show b = parens . unwords $ "Book" : [ b .# f
                                         | f <- [ title      #$ show
                                                , author     #$ show
                                                , characters #$ show ] ]

howard  = Person "Howard"  "Lovecraft" 46 Nothing
charles = Person "Charles" "Ward"      26 Nothing
marinus = Person "Marinus" "Willett"   56 Nothing
william = Person "William" "Dyer"      53 Nothing
frank   = Person "Frank"   "Pabodie"   49 Nothing
herbert = Person "Herbert" "West"      32 Nothing
abdul   = Person "Abdul"   "Alhazred"  71 Nothing

mountains    = Book "At the Mountains of Madness"     undefined []
caseOfCDW    = Book "The Case of Charles Dexter Ward" undefined []
reanimator   = Book "Herbert West -- The Re-animator" undefined []
necronomicon = Book "Necronomicon"                    undefined []

persons = [howard, charles, marinus, herbert, william, frank, abdul]

-- All the lets and primes are ugly, but it clearly shows that nothing
-- is being mutated. With the State monad, we could use (<=:) instead.
main = do print $ necronomicon .# title
          let necronomicon' = necronomicon .# author =: abdul
          print $ necronomicon' .# author # lastName

          sep

          let [mountains', caseOfCDW', reanimator' ] =
                  [mountains, caseOfCDW, reanimator ] <.#> author =: howard

              [ mountains'', caseOfCDW'', reanimator'' ] =
                  [ mountains', caseOfCDW', reanimator' ] *# characters =*
                      [ [ william, frank ]
                      , [ charles, marinus ]
                      , [ herbert ] ]
          let books = [ mountains'', caseOfCDW''
                      , reanimator'', necronomicon']
          print books

          sep

          print $ howard .# (firstName, lastName, age)
          print $ howard .# (firstName, (lastName, age )) =~
              (reverse *** reverse *** negate)

          sep

          print $ books <.#> characters <#> (lastName, firstName )
          print $ sortBy (compare `onField` author # lastName) books
          print $ sortBy (compare `onField` (characters <#> age) #$ sum) books

          sep

          print $ case charles of
                       (match lastName -> "Dexter") -> Left False
                       (match lastName -> "Ward")   -> Left True
                       (match (age, superior) -> (a, Just s))
                           | a > 18    -> Right a
                           | otherwise -> Right (s .# age)

          sep

          print $ howard .# lastName #! 0
          print $ howard .# lastName #! 0 =: Nothing
          print $ howard .# lastName #! 0 =: Just 'X'

          sep

          let frank' = frank .# superior =: Just william
          -- :: Maybe Person
          print $ frank' .# superior
          -- :: Maybe (Maybe Person)
          print $ frank' .# superior <#> superior
          -- :: Maybe (Maybe (Maybe Person))
          print $ frank' .# superior <#> superior <#> superior
          -- :: Maybe Person
          print $ frank' .# superior <##> superior <##> superior <##> superior
    where sep = putStrLn ""

