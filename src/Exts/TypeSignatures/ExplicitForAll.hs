-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/explicit_forall.html#extension-ExplicitForAll
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
module Exts.TypeSignatures.ExplicitForAll where

{-
Explicit universal quantification (forall)

When the language option ExplicitForAll is used, the keyword forall allows us to
say exactly what this means.
-}

{-
The forall-or-nothing rule
In certain forms of types, type variables obey what is known as the “forall-or-nothing” rule:
if a type has an outermost, explicit, invisible `forall`, then all of the type
variables in the type must be explicitly quantified.
-}

f  :: forall a b. a -> b -> b         -- OK, `a` and `b` are explicitly bound
f = undefined

g  :: forall a. a -> forall b. b -> b -- OK, `a` and `b` are explicitly bound
g = undefined

-- Rejected, `b` is not in scope
-- h  :: forall a. a -> b -> b
-- h = undefined

{-
In places where the forall-or-nothing rule takes effect, if a type does not have
an outermost invisible forall, then any type variables that are not explicitly
bound by a forall become implicitly quantified.
-}

i :: a -> b -> b             -- `a` and `b` are implicitly quantified
i = undefined

j :: a -> forall b. b -> b   -- `a` is implicitly quantified
j = undefined

k :: (forall a. a -> b -> b) -- `b` is implicitly quantified
k = undefined


{-
TODO: get following expression pass compiler
Hints:
LANGUAGE StandaloneKindSignatures
LANGUAGE PolyKinds

type L :: forall a -> b -> b -- `b` is implicitly quantified
-}
