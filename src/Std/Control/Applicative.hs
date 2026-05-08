{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Std.Control.Applicative
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Applicative chapter
-- Prereqs     : Std.Data.Functor
--
-- == Concept
-- An 'Applicative' is a 'Functor' where you can also lift a pure value
-- ('pure') and apply a wrapped function to a wrapped argument ('<*>').
-- It sits strictly between 'Functor' and 'Monad' in expressive power.
--
-- == Example
-- >>> (+) <$> Just 2 <*> Just 3
-- Just 5
-- >>> pure 7 :: Maybe Int
-- Just 7
--
-- == Exercise
-- Implement @liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c@
-- using only @pure@, @(<*>)@, and @(<$>)@. Then derive @(<*>)@ from
-- @liftA2@ to convince yourself they have the same power.
module Std.Control.Applicative where

import Std.Data.Functor

class Functor f => Applicative f where
  -- Laws:
  --  1. Identity
  --    pure id <*> v = v
  --  2. Composition
  --    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  --  3. Homomorphism
  --    pure f <*> pure x = pure (f x)
  --  4. Interchange
  --    u <*> pure y = pure ($ y) <*> u
  pure :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
