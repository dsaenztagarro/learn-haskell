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
