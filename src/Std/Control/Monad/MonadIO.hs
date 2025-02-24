module Std.Control.Monad.MonadIO where

import Std.Control.Monad.ExceptT
import Std.Control.Monad.StateT
import Std.Control.Monad.MonadTrans

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO
