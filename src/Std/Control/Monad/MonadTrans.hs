{-# LANGUAGE QuantifiedConstraints #-}
module Std.Control.Monad.MonadTrans where

import Prelude

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a -> t m a

