-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exts.Records.RecordWildCards where

-- Allows to take all of the fields of a record and turn them into variables that
-- are bound inside of an expression by using `TypeConstructor {..}`

data C = C { a :: Int, b :: Int, c :: Int } deriving (Show)

-- without extension
sumC C { a = 1, b = b, c = c } = b + c

-- in a record pattern, each elided field f is replaced by the pattern f = f.
sumC' C { a = 1, .. } = b + c

-- in an expression, when constructing a record...

-- ... with field names defined as let bindings
newC = let { a = 1; b = 2; c = 3 } in C {..}  -- Expands to C { a = a, b = b, c = c }

-- ... with field names defined as function parameters
newC' a b = let { c = 3 } in C {..}
