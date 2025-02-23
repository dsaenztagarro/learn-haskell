{-# LANGUAGE NoImplicitPrelude #-}
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
