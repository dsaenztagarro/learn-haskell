module Libs.Mtl.Control.Monad.State where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f g = State $ \lastState ->
    let (val, nextState) = runState g lastState
    in (f val, nextState)

instance Applicative (State s) where
  pure val = State $ \s -> (val, s)
  f <*> g = State $ \oldState ->
    let (h, funcState)  = runState f oldState
        (val, valState) = runState g funcState
    in (h val, valState)

instance Monad (State s) where
  return = pure
  f >>= g = State $ \oldState ->
    let (val, valState) = runState f oldState
    in runState (g val) valState  -- val <- f

evalState :: State s a -> s -> a
evalState stateAction initialState =
  fst $ runState stateAction initialState

execState :: State s a -> s -> s
execState stateAction initialState =
  snd $ runState stateAction initialState

put :: s -> State s ()
put state = State $ \_ -> ((), state)

get :: State s s
get = State $ \state -> (state, state)
