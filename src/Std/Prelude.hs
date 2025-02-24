{-# LANGUAGE NoImplicitPrelude #-}

module Std.Prelude

import Prelude (const, Int, even, succ)
import qualified Text.Read as Text

-- Effective Haskell (page 464)
class Monad m => MonadFail where
  -- Law:
  -- fail a >>= b == fail a
  fail :: String -> m a

-- empty @Maybe
-- Nothing
