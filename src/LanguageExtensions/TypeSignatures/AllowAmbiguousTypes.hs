-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ambiguous_types.html
{-# LANGUAGE AllowAmbiguousTypes #-}

module LanguageExtensions.TypeSignatures.AllowAmbiguousTypes where

{-
Allow type signatures which appear that they would result in an unusable binding.

Each user-written type signature is subjected to an ambiguity check.

The ambiguity check rejects functions that can never be called. For example:
-}

class C a where

f :: C a => Int
f = 0
