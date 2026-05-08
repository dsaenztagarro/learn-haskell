{-# LANGUAGE QuantifiedConstraints #-}
-- |
-- Module      : Libs.Mtl.Control.Monad.MonadTrans
-- Stage       : 07-Mtl  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Monad transformers / mtl chapter
--               transformers package — Control.Monad.Trans.Class
-- Prereqs     : Libs.Mtl.Control.Monad.State
--
-- == Concept
-- The class that says "I am a monad transformer". 'lift' embeds an
-- action of the inner monad @m@ into the outer transformer @t m@.
-- The 'QuantifiedConstraints' superclass guarantees that for /any/
-- inner @Monad m@ the result @t m@ is itself a 'Monad' — without it,
-- every transformer would have to repeat the constraint at each use
-- site.
module Libs.Mtl.Control.Monad.MonadTrans where

import Prelude

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a -> t m a

