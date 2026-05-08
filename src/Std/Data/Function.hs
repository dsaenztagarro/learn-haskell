{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Std.Data.Function
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Functor/Applicative chapters
-- Prereqs     : (none — this is the entry point of Stage 01)
--
-- == Concept
-- The function arrow @(->) a@ is itself a 'Functor' and 'Applicative'.
-- Wrapping it in a newtype ('Function') lets us write the instances without
-- the orphan-instance issue that would arise on the bare @(->)@.
--
-- == Example
-- >>> runFunction (fmap (+1) (Function (*2))) 3
-- 7
-- >>> runFunction (Function (+) <*> Function (*2)) 5
-- 15  -- (5+) <*> (5*2) = 5 + 10
--
-- == Exercise
-- Add a 'Monad' instance for 'Function' a. Hint: this is the 'Reader' monad.
module Std.Data.Function where

import Std.Data.Functor
import Std.Control.Applicative
import Prelude (const, (.), ($))

newtype Function a b = Function { runFunction :: a -> b }

instance Functor (Function a) where
  -- fmap :: (b -> c) -> Function a b -> Function a c
  fmap f (Function g) = Function (f . g)

instance Applicative (Function a) where
  pure a = Function $ const a
  -- (<*>) :: Function a (b -> c) -> Function a b -> Function a c
  Function f <*> Function g = Function $ \value -> f value (g value)

