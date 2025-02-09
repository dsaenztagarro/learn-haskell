{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LanguageExtensions.Records.RecordWildCards where

data C = C { a :: Int, b :: Int, c :: Int } deriving (Show)

-- without extension
sumC C { a = 1, b = b, c = c } = b + c

-- Usage of “..”

-- in a record pattern, each elided field f is replaced by the pattern f = f.
sumC' C { a = 1, .. } = b + c

-- in an expression, when constructing a record
newC = let { a = 1; b = 2; c = 3 } in C {..}
