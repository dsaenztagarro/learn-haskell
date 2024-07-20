module Base.Monad.MonadFail where

class Monad m => MonadFail m where
  fail :: String -> m a

{-
LAW: fail a >>= b == fail a
-}
