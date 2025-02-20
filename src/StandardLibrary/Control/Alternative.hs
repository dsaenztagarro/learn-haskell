{-# LANGUAGE NoImplicitPrelude #-}
module StandardLibrary.Control.Alternative where

import StandardLibrary.Control.Applicative

-- Effective Haskell (page 457)
-- Control.Applicative in base
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL empty, (<|>), some, many #-}
