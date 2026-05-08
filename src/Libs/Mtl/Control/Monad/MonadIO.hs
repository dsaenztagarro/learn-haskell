-- |
-- Module      : Libs.Mtl.Control.Monad.MonadIO
-- Stage       : 07-Mtl  (see docs/ROADMAP.md)
-- Source      : base — Control.Monad.IO.Class
-- Prereqs     : Libs.Mtl.Control.Monad.MonadTrans
--
-- == Concept
-- @liftIO@ is the standard way to embed an 'IO' action into any
-- transformer stack ending in 'IO'. The two instances below show the
-- pattern: every transformer composes @lift . liftIO@.
module Libs.Mtl.Control.Monad.MonadIO where

import Libs.Mtl.Control.Monad.ExceptT
import Libs.Mtl.Control.Monad.StateT
import Libs.Mtl.Control.Monad.MonadTrans

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO
