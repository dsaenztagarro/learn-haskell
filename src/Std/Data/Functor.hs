{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Std.Data.Functor
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : EH:ch? functor
-- Prereqs     : Std.Data.Function
--
-- == Concept
-- A 'Functor' is anything you can 'fmap' over while preserving structure.
-- The two laws below are what distinguish a real functor from a function
-- with the right shape.
--
-- == Example
-- >>> fmap (+1) (Just 2)
-- Just 3
-- >>> (*2) <$> [1,2,3]
-- [2,4,6]
--
-- == Exercise
-- Define an instance @Functor Tree@ for @data Tree a = Leaf | Node a (Tree a) (Tree a)@.
-- Verify by hand that it satisfies the identity law.
module Std.Data.Functor where

import Prelude (const)

class Functor f where
  -- Laws:
  --  1. Identity
  --    fmap id f = id f
  --  2. Composition
  --    fmap (f . g) = fmap f . fmap g
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) a fb = fmap (const a) fb

infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
