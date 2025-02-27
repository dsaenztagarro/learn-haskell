module Std.Data.Monoid where

import Prelude (Semigroup)

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
