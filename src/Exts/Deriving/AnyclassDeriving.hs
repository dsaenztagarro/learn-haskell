{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |
-- Module      : Exts.Deriving.AnyclassDeriving
-- Stage       : 03-Deriving  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Deriving strategies chapter
--               GHC users guide — DeriveAnyClass
--
-- == Concept
-- @DeriveAnyClass@ + @DefaultSignatures@ lets a class ship a default
-- method using a constraint stronger than the class's own
-- (here: @Show a => a -> String@). Any instance can then be derived
-- *empty* and pick up that default.
--
-- == Example
-- >>> redacted (UserName "alice")
-- "\"alice\""    -- via the default Show-based implementation
module Exts.Deriving.AnyclassDeriving where

{-
Haskell 98 allows you to define a default implementation when declaring a class:

class Redacted a where
  redacted :: a -> String
  redacted = "Untitled"

The type of the `redacted` method is `a -> String`, and this is also the type of
the default method. You can lift this restriction and give another type to the
default method using the flag -XDefaultSignatures.
-}

class Redacted a where
  redacted :: a -> String
  default redacted :: Show a => a -> String
  redacted = show

{-
`DeriveAnyClass` extension allows us to derive any class that we want, and GHC
will add an empty instance declaration for us.
-}
newtype UserName = UserName String deriving (Show, Redacted)

