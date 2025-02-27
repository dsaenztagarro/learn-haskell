module Std.Data.Semigroup where

class Semigroup a where
  (<>) :: a -> a -> a
