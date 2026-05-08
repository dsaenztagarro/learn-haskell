-- |
-- Module      : Std.Data.Semigroup
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Semigroup/Monoid chapter
-- Prereqs     : (none)
--
-- == Concept
-- A 'Semigroup' is a type with an associative binary operation @(<>)@.
-- @a <> (b <> c) ≡ (a <> b) <> c@. That's the only law.
--
-- == Example
-- >>> [1,2] <> [3,4]
-- [1,2,3,4]
-- >>> "foo" <> "bar"
-- "foobar"
--
-- == Exercise
-- Find two functions on @[Int]@ that have the right type for @(<>)@ but
-- are /not/ associative. (Hint: @\\xs ys -> head xs : ys@ is one.)
module Std.Data.Semigroup where

class Semigroup a where
  (<>) :: a -> a -> a
