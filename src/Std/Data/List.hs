{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Std.Data.List
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : EH:ch? functor-applicative-monad
-- Prereqs     : Std.Data.Functor, Std.Control.Applicative, Std.Control.Monad
--
-- == Concept
-- A hand-rolled cons-list with 'Functor', 'Applicative', and 'Monad'
-- instances. The @Applicative@ for list is /Cartesian/ — applying a list of
-- functions to a list of arguments yields every combination — which is also
-- what @>>=@ does (\"non-determinism monad\").
--
-- == Example
-- >>> fromList (toList [1,2] >>= \x -> toList [x, x*10])
-- [1,10,2,20]
--
-- == Exercise
-- Prove that the list 'Applicative' satisfies the homomorphism law
-- @pure f \<*\> pure x = pure (f x)@ given the definition above.
module Std.Data.List where

import Std.Data.Functor
import Std.Control.Applicative
import Std.Control.Monad
import Prelude (show, (.), Show)

data List a = Empty | List a (List a)

instance Show a => Show (List a) where
  show = show . fromList

instance Functor List where
  fmap _ Empty = Empty
  fmap f (List a as) = List (f a) (fmap f as)

instance Applicative List where
  pure a = List a Empty
  Empty <*> _ = Empty
  List f fs <*> vals = (f <$> vals) `concatList` (fs <*> vals)

instance Monad List where
  return = pure
  Empty >>= _f = Empty
  List a as >>= f = (f a) `concatList` (as >>= f)

toList :: [a] -> List a
toList [] = Empty
toList (a:as) = List a (toList as)

fromList :: List a -> [a]
fromList Empty = []
fromList (List a as) = a : fromList as

concatList :: List a -> List a -> List a
concatList Empty as = as
concatList (List a as) bs = List a (concatList as bs)
