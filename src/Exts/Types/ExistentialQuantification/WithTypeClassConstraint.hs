{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Exts.Types.ExistentialQuantification.WithTypeClassConstraint
-- Stage       : 05-Existentials  (see docs/ROADMAP.md)
-- Source      : EH:ch? existentials
-- Prereqs     : Exts.Types.ExistentialQuantification
--
-- == Concept
-- The most idiomatic existential pattern: a constructor packaged with
-- a typeclass constraint, so the consumer can call any class method
-- without knowing the underlying type. Compare to
-- 'Exts.Types.ExistentialQuantification.WithRecord' (which packages
-- functions instead of a class dictionary).
--
-- == Example
-- >>> showWhatCanBeShown (CanBeShown (42 :: Int))
-- "42"
module Exts.Types.ExistentialQuantification.WithTypeClassConstraint where

data CanBeShown = forall a. Show a => CanBeShown a

showWhatCanBeShown :: CanBeShown -> String
showWhatCanBeShown (CanBeShown value) = show value

instance Show CanBeShown where
  show (CanBeShown a) = show a
