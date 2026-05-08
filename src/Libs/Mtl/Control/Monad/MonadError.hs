{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Libs.Mtl.Control.Monad.MonadError
-- Stage       : 07-Mtl  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — mtl chapter
--               mtl package — Control.Monad.Except.Class
-- Prereqs     : Libs.Mtl.Control.Monad.ExceptT,
--               Libs.Mtl.Control.Monad.MonadState (for the class pattern)
--
-- == Concept
-- The mtl-style class @MonadError e m | m -> e@: any monad that can
-- throw an error of a given type and catch it. The instance for
-- @StateT s m@ shows how to /lift/ catch through another transformer
-- — boilerplate that the class hides from callers.
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

