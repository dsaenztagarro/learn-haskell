{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Exts.TypeSignatures.ExplicitForAll  (NB: see warning below)
-- Stage       : 04-TypeSignatures  (see docs/ROADMAP.md)
-- Source      : GHC users guide — FlexibleContexts (snippet only)
--
-- == Concept
-- /Snippet/ — the canonical demo lives at
-- 'Exts.TypeClasses.FlexibleContexts'. This file is a leftover draft:
-- the path says @TypeSignatures/FlexibleContexts.hs@ but the module
-- declaration says @Exts.TypeSignatures.ExplicitForAll@, so it is
-- /not/ exposed by the cabal library and should not be imported.
-- Kept here as a reading example; if you want the real exposed module
-- open the @TypeClasses@ counterpart instead.
module Exts.TypeSignatures.ExplicitForAll where

{-
The `FlexibleContexts` extension lifts the Haskell 98 restriction that the
type-class constraints (anywhere they appear) must have the form
(class type-variable) or (class (type-variable type1 type2 … typen)).
With `FlexibleContexts` these type signatures are perfectly okay:

g :: Eq [a] => ...
g :: Ord (T a ()) => ...

This extension does not affect equality constraints in an instance context;
they are permitted by `TypeFamilies` or `GADTs`.

Note that `FlexibleContexts` affects usages of class constraints, in type
signatures and other contexts. In contrast, `FlexibleInstances` loosens a
similar restriction in place when declaring a new instance.
-}

g :: Eq [a] => a -> a
g = undefined
