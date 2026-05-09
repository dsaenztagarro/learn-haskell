{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      : Libs.Mtl.Control.Monad.MonadState
-- Stage       : 07-Mtl  (see docs/ROADMAP.md)
-- Source      : EH:ch? mtl
--               mtl package — Control.Monad.State.Class
-- Prereqs     : Libs.Mtl.Control.Monad.StateT,
--               Exts.FunctionalDependency.ShellCmd
--
-- == Concept
-- The mtl-style /class/ for "any monad that supports get/put". With
-- the functional dependency @m -> s@, GHC infers the state type from
-- the monad — that's what lets you write @get@ inside a transformer
-- stack without disambiguating which layer holds the state.
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
