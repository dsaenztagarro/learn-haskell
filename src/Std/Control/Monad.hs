{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Std.Control.Monad
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Monad chapter
-- Prereqs     : Std.Control.Applicative
--
-- == Concept
-- A 'Monad' adds /sequencing/ to 'Applicative': the second computation can
-- depend on the result of the first. The mechanical operator is @>>=@
-- (\"bind\"); 'join' shows the equivalent presentation in terms of nesting.
--
-- == Example
-- >>> Just 3 >>= (\x -> Just (x + 1))
-- Just 4
-- >>> join (Just (Just 5))
-- Just 5
--
-- == Exercise
-- Define @Monad@ for @data Pair a = Pair a a@ — or prove it cannot satisfy
-- the laws and explain why.
module Std.Control.Monad where

-- See $GHC/libraries/ghc-internal/src/GHC/Internal/Base.hs

import Std.Control.Applicative
import Prelude (id)

class Applicative m => Monad m where
  -- Laws:
  --  1. Left identity
  --    return a >>= m = m a
  --  2. Right identity
  --    m >>= return = m
  --  3. Associativity
  --    (a >>= b) >>= c = a >>= (\x -> b x >>= c)
  infixl 1 >>=
  (>>=) :: m a -> (a -> m b) -> m b
  infixl 1 >>
  (>>) :: m a -> m b -> m b
  a >> b = a >>= \_ -> b
  return :: a -> m a

join :: Monad m => m (m a) -> m a
join m = m >>= id
