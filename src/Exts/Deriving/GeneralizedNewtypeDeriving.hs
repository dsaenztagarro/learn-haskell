-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html#newtype-deriving
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Exts.Deriving.GeneralizedNewtypeDeriving
-- Stage       : 03-Deriving  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Deriving strategies chapter
--               https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html
--
-- == Concept
-- A newtype shares its runtime representation with the wrapped type, so
-- GHC can /lift/ existing instances (e.g. 'Num', 'Ord') from the inner
-- type to the newtype with zero overhead. See 'Base.Roles' for when this
-- is unsafe.
--
-- == Example
-- >>> USD 100 + USD 50
-- USD {getMillis = 150}
module Exts.Deriving.GeneralizedNewtypeDeriving where

{-
Using `GeneralizedNewtypeDeriving` (Generalised derived instances for newtypes),
a programmer can take existing instances of classes and “lift” these into
instances of that class for a newtype.

`newtype` offers a zero-overhead abstraction, and the compiler guarantees that
the newtype will store its value in the same way as the underlying type that
it's wrapping.

In GHC, two types that have the same underlying runtime representation are
considered representationally equal. When two types are representationally
equal, the compiler can figure out how to safely coerce values from one type to
the other.
-}

newtype USD = USD { getMillis :: Integer } deriving (Eq,Ord,Show,Num)

