{-# LANGUAGE TypeFamilies #-}

{-
This extension let us use the same types we've been writing at the term level
as kinds when we are working at the type level.

With this extension enabled, every type we've defined is also a kind, and the
constructors for that type are its inhabitants.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- Allow us using our own own operators to use at the type level
{-# LANGUAGE TypeOperators #-}

{- The following 2 extensions work together: -}

{- Allows using explicit universal quantification `forall a`. Using an explicit
   `forall` brings the type variables into scope in the body of the function.
-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Gives you the ability to pass type names as arguments to polymorphic functions,
to select the type class instance that's used.

read @Integer "1"
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module LanguageExtensions.Kinds.OpenTypeFamily where
{- This module defines the kinds we'll be working with, and a number of utility
   functions to help us integrate our type level code with term level code.

   type family AppendSymbol (a :: Symbol) (b :: Symbol) :: Symbol where ...

   symbolVal :: KnownSymbol n => proxy n -> String

   The KnownSymbol constraint is a type class constraint that's defined for all
   Symbol types. This means we know that `n` must be some kind of type level
   string.
-}
import GHC.TypeLits

-- Allow us to write `Type` instead of `*` when using kind signatures
import Data.Kind
import Data.Proxy

type family NamedType (a :: Type) :: Symbol
type instance NamedType Int = "Int"
type instance NamedType Char = "Char"
{- Type family instances are allowed to overlap only if all of the overlapping
   instances would return exactly the same value in the overlapping cases.

   Values need to be structurally equal without additional normalization.

   String overlaps with type family instance for list type
-}
type instance NamedType String = "[" :++: NamedType Char :++: "]"
type instance NamedType (a -> b) =
  NamedType a `AppendSymbol` " -> " `AppendSymbol` NamedType b

{- type (:++:) = AppendSymbol

   A type family must always have all of its arguments applied

   src/LanguageExtensions/OpenTypeFamilies.hs:58:1: error:
       • The type family ‘AppendSymbol’ should have 2 arguments, but has been given none
       • In the type synonym declaration for ‘:++:’
      |
   58 | type (:++:) = AppendSymbol
      | ^^^^^^^^^^^^^^^^^^^^^^^^^^
   Error: cabal: Failed to build effective-haskell-0.1.0.0.
-}

type (:++:) a b = a `AppendSymbol` b

type instance NamedType (a, b) = "(" :++: NamedType a :++: "," :++: NamedType b :++: ")"
type instance NamedType [a] = "[" :++: NamedType a :++: "]"

showTypeName :: forall t. (KnownSymbol (NamedType t)) => String
showTypeName = symbolVal $ Proxy @(NamedType t)
