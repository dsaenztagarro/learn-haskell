{-# LANGUAGE TupleSections #-}
module Base.Monad.StateT where

import Control.Applicative
import Base.Monad.Identity

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
type State s = StateT s Identity

evalState :: State s a -> s -> a
evalState stateAction initialState =
  runIdentity $ evalStateT stateAction initialState

execState :: State s a -> s -> s
execState stateAction initialState =
  runIdentity $ execStateT stateAction initialState

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT stateAction initialState =
  fst <$> runStateT stateAction initialState

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT stateAction initialState =
  snd <$> runStateT stateAction initialState

instance Functor m => Functor (StateT s m) where
  fmap f s =
    StateT $ fmap (first f) . runStateT s
    where first g (a, b) = (g a, b)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  f <*> a = StateT $ \s -> do
    (g,s') <- runStateT f s
    (b,s'') <- runStateT a s'
    pure (g b, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  a >>= f = StateT $ \s -> do
    (b, s') <- runStateT a s
    runStateT (f b) s'

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT $ const empty
  a <|> b = StateT $ \s ->
    runStateT a s <|> runStateT b s

put :: Monad m => s -> StateT s m ()
put state = StateT $ \_ -> pure ((), state)

get :: Monad m => StateT s m s
get = StateT $ \state -> pure (state, state)

liftStateT :: Monad m => m a -> StateT s m a
liftStateT a = StateT $ \s -> (, s) <$> a
