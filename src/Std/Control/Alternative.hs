{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Std.Control.Alternative
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : EH:p457 alternative
-- Prereqs     : Std.Control.Applicative
--
-- == Concept
-- 'Alternative' is to 'Applicative' what 'Monoid' is to a plain type:
-- it adds a binary choice operator @<|>@ and an identity 'empty'. Used
-- everywhere parser combinators do "try this, otherwise that".
--
-- == Example
-- >>> Nothing <|> Just 1 <|> Just 2
-- Just 1
-- >>> empty :: Maybe Int
-- Nothing
--
-- == Exercise
-- Define @optional :: Alternative f => f a -> f (Maybe a)@ (see the
-- commented-out hint at the bottom of the file). When does it differ from
-- @fmap Just@?
module Std.Control.Alternative where

import Std.Control.Applicative
import Std.Data.Functor

-- Effective Haskell (page 457)
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL empty, (<|>), some, many #-}

-- optional :: Alternative f => f a -> f (Maybe a)
-- optional v = Just <$> v <|> pure Nothing
