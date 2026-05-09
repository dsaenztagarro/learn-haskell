{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Std.Data.Maybe
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : EH:ch? functor-applicative-monad
-- Prereqs     : Std.Data.Functor, Std.Control.Applicative, Std.Control.Monad
--
-- == Concept
-- 'Maybe' is the canonical \"value or nothing\" type, and the simplest
-- non-trivial 'Functor', 'Applicative', and 'Monad' to write from
-- scratch. The @half@ and @bound@ helpers below are the classic Effective
-- Haskell example of monadic chaining.
--
-- == Example
-- >>> half 10
-- Just 5
-- >>> bound (0, 20) 11 >>= return . succ >>= half
-- Just 6
--
-- == Exercise
-- Without using @do@-notation or @>>=@, rewrite the chain
-- @bound (0,20) 11 >>= return . succ >>= half@ using @<*>@ and @fmap@
-- only, or argue why it cannot be done.
module Std.Data.Maybe where

import Std.Data.Functor
import Std.Control.Applicative
import Std.Control.Monad
import Prelude (Int, (>=), (<=), (&&), even)

data Maybe a = Nothing | Just a

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just f <*> a = f <$> a

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just a >>= f = f a

half :: Int -> Maybe Int
half num =
  if even num
  then Just num
  else Nothing

bound :: (Int, Int) -> Int -> Maybe Int
bound (min, max) num =
  if (num >= min) && (num <= max)
  then Just num
  else Nothing
