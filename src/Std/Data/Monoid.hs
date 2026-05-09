-- |
-- Module      : Std.Data.Monoid
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : EH:ch? semigroup-monoid
-- Prereqs     : Std.Data.Semigroup
--
-- == Concept
-- A 'Monoid' is a 'Semigroup' with a neutral element 'mempty'. Two extra
-- laws beyond associativity: @mempty <> x ≡ x@ and @x <> mempty ≡ x@.
--
-- == Example
-- >>> mempty :: [Int]
-- []
-- >>> mconcat ["a", "b", "c"]
-- "abc"
--
-- == Exercise
-- Define a 'Monoid' instance for @newtype Endo a = Endo (a -> a)@.
-- What is the identity element?
module Std.Data.Monoid where

import Std.Data.Semigroup

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
