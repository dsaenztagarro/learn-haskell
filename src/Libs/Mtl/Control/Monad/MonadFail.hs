-- |
-- Module      : Libs.Mtl.Control.Monad.MonadFail
-- Stage       : 07-Mtl  (see docs/ROADMAP.md)
-- Source      : Effective Haskell, page 464
--               base — Control.Monad.Fail
--
-- == Concept
-- The class behind a failed do-notation pattern match (e.g.
-- @Just x <- ...@ when the action returns @Nothing@). Pulling it out
-- of 'Monad' (which it used to be a method of) means non-failing
-- monads no longer need to provide a @fail@ that just calls 'error'.
module Libs.Mtl.Control.Monad.MonadFail where

-- Effective Haskell (page 464)
class Monad m => MonadFail m where
  -- Law:
  -- fail a >>= b == fail a
  fail :: String -> m a

