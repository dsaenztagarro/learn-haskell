{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- GHC tracks two different sorts of type variables: specified types and
   inferred types.

   When we manually write out the type variables for the functions, they become
   specified types. When functions have polymorphic type variables that we
   never directly reference, they are tracked by the compiler as inferred types.

   It turns out, we're only allowed to use visible type applications to select
   specified types.

   :set -fprint-explicit-foralls

   convertViaInt a = fromIntegral $ fromIntegral @_ @Int a

   :t convertViaInt
   convertViaInt :: forall {w} {b}. (Integral w, Num b) => w -> b

   `{w}` and `{b}` are inferred types, so it is not possible to perform type
   applications in the previous function definition.
-}
-- |
-- Module      : Exts.InferredTypes
-- Stage       : 04-TypeSignatures  (see docs/ROADMAP.md)
-- Source      : EH:ch12 type-level
--
-- == Concept
-- GHC distinguishes /specified/ from /inferred/ type variables. Only
-- specified ones can be selected with visible type application
-- (@\@T@). Wrap a variable in braces (@{a}@) in a signature to mark it
-- inferred; this is a deliberate API choice — callers cannot fix it.
--
-- == Example
-- >>> convertViaInt' (3 :: Int) :: Double
-- 3.0
module Exts.InferredTypes where

convertViaInt :: forall {a} b. (Integral a, Num b) => a -> b
convertViaInt input =
  fromIntegral $ fromIntegral @_ @Int input

convertViaInt' :: forall {a} b. (Integral a, Num b) => a -> b
convertViaInt' input =
  fromIntegral @_ @b $ fromIntegral @_ @Int input
