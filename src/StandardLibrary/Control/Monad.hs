{-# LANGUAGE NoImplicitPrelude #-}
module StandardLibrary.Control.Monad where

import StandardLibrary.Control.Applicative

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
