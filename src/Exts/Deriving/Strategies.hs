{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
-- |
-- Module      : Exts.Deriving.Strategies
-- Stage       : 03-Deriving  (see docs/ROADMAP.md)
-- Source      : EH:ch? deriving, GHC:deriving-strategies
--               GHC users guide — DerivingStrategies
-- Prereqs     : Exts.Deriving.GeneralizedNewtypeDeriving,
--               Exts.Deriving.AnyclassDeriving,
--               Exts.Deriving.ViaCompatibleType
--
-- == Concept
-- The umbrella for the four strategies — @stock@, @newtype@, @anyclass@,
-- @via@ — that decide /how/ a deriving clause synthesises an instance.
-- Without an explicit strategy, GHC picks one and the choice can be
-- surprising; @DerivingStrategies@ forces you (and future readers) to
-- spell it out.
--
-- == Example
-- The single newtype @AdminUser@ uses three strategies at once: @Show@
-- via @stock@, @Redacted@ lifted via @newtype@, @Cool@ generated empty
-- via @anyclass@. Compare each instance and notice which uses the
-- wrapped type's behaviour vs. its own.
module Exts.Deriving.Strategies where

class Redacted a where
  redacted :: a -> String
  default redacted :: Show a => a -> String
  redacted = show

class Show a => Cool a where
  cool :: a -> String
  default cool :: a -> String
  cool a = "Cool! " <> show a

newtype UserName = UserName String deriving Show
instance Redacted UserName where
  redacted (UserName user) = "UserName: " <> user

newtype AdminUser = AdminUser UserName
  deriving stock Show
  deriving newtype Redacted
  deriving anyclass Cool

{-
With the `DerivingStrategies` extension we can add several deriving clauses when
we define a type, and each of those can have an associated deriving strategy.
There are four strategies you can use:
- `stock` can be used if you are trying to derive one of the standard derivable
  type classes and want to use the standard approach defined in the Haskell
  standard.
- `newtype` tells the compiler to use the generalized newtype deriving strategy
  to create an instance based on a newtype wrapper.
- `anyclass` tells the compiler to generate and empty instance declaration,
  which is the default when DeriveAnyClass is enabled.
- `via` uses the deriving via strategy.
-}
