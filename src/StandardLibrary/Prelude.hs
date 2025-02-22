{-# LANGUAGE NoImplicitPrelude #-}

module StandardLibrary.Prelude

import Prelude (const, Int, even, succ)
import qualified Text.Read as Text

-- Effective Haskell (page 457)
-- Control.Applicative in base
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL (<|>) #-}

-- Effective Haskell (page 464)
class Monad m => MonadFail where
  -- Law:
  -- fail a >>= b == fail a
  fail :: String -> m a

-- empty @Maybe
-- Nothing
