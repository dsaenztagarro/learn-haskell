{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Std.Control.Monad.MonadState where

import qualified Std.Control.Monad.StateT as StateT
import Std.Control.Monad.ExceptT
import Std.Control.Monad.MonadTrans

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance Monad m => MonadState s (StateT.StateT s m) where
  get = StateT.get
  put = StateT.put

instance MonadState s m => MonadState s (ExceptT e m) where
  get = lift get
  put = lift . put
