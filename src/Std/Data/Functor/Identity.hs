-- |
-- Module      : Std.Data.Functor.Identity
-- Stage       : 01-Std  (see docs/ROADMAP.md)
-- Source      : EH:ch? functor-applicative
-- Prereqs     : Std.Data.Functor, Std.Control.Applicative
--
-- == Concept
-- The trivial functor: 'Identity' wraps a value and applies the function
-- straight through. Useful as the base of monad transformer stacks
-- (@StateT s Identity ≡ State s@) and as a sanity-check baseline when
-- writing new instances.
--
-- == Example
-- >>> runIdentity (fmap (+1) (Identity 41))
-- 42
-- >>> runIdentity ((+) <$> Identity 2 <*> Identity 3)
-- 5
--
-- == Exercise
-- Show that @Identity@ satisfies all three monad laws on paper, using only
-- the definitions in this module.
module Std.Data.Functor.Identity where

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f a = Identity $ f (runIdentity a)

instance Applicative Identity where
  pure = Identity
  f <*> a = Identity $ runIdentity f $ runIdentity a

instance Monad Identity where
  return = pure
  a >>= f = f (runIdentity a)
