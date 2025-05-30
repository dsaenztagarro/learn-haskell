{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Libs.Mtl.Control.Monad.MonadError where

import qualified Libs.Mtl.Control.Monad.ExceptT as ExceptT
import Libs.Mtl.Control.Monad.MonadTrans
import Libs.Mtl.Control.Monad.StateT

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

