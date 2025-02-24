{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Control.Monad.MonadError where

import qualified Std.Control.Monad.ExceptT as ExceptT
import Std.Control.Monad.MonadTrans
import Std.Control.Monad.StateT

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance Monad m => MonadError e (ExceptT.ExceptT e m) where
  throwError = ExceptT.throwError
  catchError = flip ExceptT.catchError

instance MonadError e m => MonadError e (StateT s m) where
  throwError = lift . throwError
  catchError action handler =
    StateT $ \s ->
      let innerAction = runStateT action s
          liftedHandler e = runStateT (handler e) s
      in catchError innerAction liftedHandler

