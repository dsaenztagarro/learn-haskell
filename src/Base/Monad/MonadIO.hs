module Base.Monad.MonadIO where

import Base.Monad.ExceptT
import Base.Monad.StateT
import Base.Monad.MonadTrans

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO
