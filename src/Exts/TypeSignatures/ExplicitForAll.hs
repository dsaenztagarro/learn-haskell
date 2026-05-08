{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Exts.TypeSignatures.ExplicitForAll
-- Stage       : 04-TypeSignatures  (see docs/ROADMAP.md)
-- Source      : GHC users guide — ExplicitForAll
--               https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/explicit_forall.html
--
-- == Concept
-- @ExplicitForAll@ allows the @forall@ keyword to appear in user-written
-- type signatures. Combined with the /forall-or-nothing rule/: if any
-- @forall@ is written at the outermost position, /every/ free type
-- variable in the type must be bound by an explicit quantifier.
--
-- == Example
-- See the @f@, @g@, @i@, @j@, @k@ definitions below — together they
-- cover the four interesting cases (all-explicit, partial, none,
-- nested).
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
The line below is /deliberately commented out/. It illustrates a kind
signature that requires more than just @ExplicitForAll@:

  type L :: forall a -> b -> b

To make it accepted you need both 'StandaloneKindSignatures' (so a
top-level @type Foo :: ...@ kind signature is parseable) and
'PolyKinds' (so @b@ may be quantified at any kind). Even with both
extensions, you still need a corresponding @type family L@ or @type L@
declaration that matches the kind. Kept commented to keep this module
focused on the @ExplicitForAll@ rules at the term level.
-}
