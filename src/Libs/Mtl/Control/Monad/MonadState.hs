{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Libs.Mtl.Control.Monad.MonadState where

import qualified Libs.Mtl.Control.Monad.StateT as StateT
import Libs.Mtl.Control.Monad.ExceptT
import Libs.Mtl.Control.Monad.MonadTrans

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance Monad m => MonadState s (StateT.StateT s m) where
  get = StateT.get
  put = StateT.put

instance MonadState s m => MonadState s (ExceptT e m) where
  get = lift get
  put = lift . put
