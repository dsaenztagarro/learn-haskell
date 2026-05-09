-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_puns.html
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- |
-- Module      : Exts.Records.NamedFieldPuns
-- Stage       : 03-Records  (see docs/ROADMAP.md)
-- Source      : GHC:named-field-puns
--               https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_puns.html
--
-- == Concept
-- @NamedFieldPuns@ lets you write @C { a, b, c }@ instead of
-- @C { a = a, b = b, c = c }@ — both at construction and pattern-match.
--
-- == Example
-- >>> newC
-- C {a = 1, b = 2, c = 3}
module Exts.Records.NamedFieldPuns where

data C = C { a :: Int, b :: Int, c :: Int } deriving (Show)

-- in an expression, when constructing a record
newC = let { a = 1; b = 2; c = 3 } in C {a, b, c}
