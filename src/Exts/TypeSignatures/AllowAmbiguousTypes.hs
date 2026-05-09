-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ambiguous_types.html
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Exts.TypeSignatures.AllowAmbiguousTypes
-- Stage       : 04-TypeSignatures  (see docs/ROADMAP.md)
-- Source      : GHC:allow-ambiguous-types
--               https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ambiguous_types.html
--
-- == Concept
-- GHC's ambiguity check normally rejects signatures with type variables
-- that callers can never pin down (e.g. @f :: C a => Int@). Enabling
-- this extension lets such a signature through, which is useful when
-- the caller pins the variable via @TypeApplications@ (@f \@MyType@).
module Exts.TypeSignatures.AllowAmbiguousTypes where

{-
Allow type signatures which appear that they would result in an unusable binding.

Each user-written type signature is subjected to an ambiguity check.

The ambiguity check rejects functions that can never be called. For example:
-}

class C a where

f :: C a => Int
f = 0
