{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Base.Monad.MonadState where

import qualified Base.Monad.StateT as StateT
import Base.Monad.ExceptT
import Base.Monad.MonadTrans

class Monad m => MonadState s m where
  get :: m s
  put :: s -> m ()

instance Monad m => MonadState s (StateT.StateT s m) where
  get = StateT.get
  put = StateT.put

instance MonadState s m => MonadState s (ExceptT e m) where
  get = lift get
  put = lift . put
