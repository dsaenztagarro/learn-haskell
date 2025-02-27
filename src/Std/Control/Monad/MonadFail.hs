module Std.Control.Monad.MonadFail where

-- Effective Haskell (page 464)
class Monad m => MonadFail m where
  -- Law:
  -- fail a >>= b == fail a
  fail :: String -> m a

