{-# LANGUAGE NoImplicitPrelude #-}
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
