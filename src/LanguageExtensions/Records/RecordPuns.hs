{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LanguageExtensions.Records.RecordPuns where

data C = C { a :: Int, b :: Int, c :: Int } deriving (Show)

-- in an expression, when constructing a record
newC = let { a = 1; b = 2; c = 3 } in C {a, b, c}
