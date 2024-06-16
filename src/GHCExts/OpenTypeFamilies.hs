{-# LANGUAGE TypeFamilies #-}

{- This extension let us use the same types we've been writing at the term level
   as kinds when we are working at the type level.

   With this extension enabled, every type we've defined is also a kind, and
   the constructors for that type are its inhabitants.
-}
{-# LANGUAGE DataKinds #-}

{- https://en.wikipedia.org/wiki/Undecidable_problem
   An undecidable problem is a decision problem for which it is proved to be
   impossible to construct an algorithm that always leads to a correct
   yes-or-no answer.

   The UndecidableInstances extension lifts restrictions that ensure the type
   checker can actually complete type checking type class instances.

   These checks are helpful more often than not, and prevent you from writing
   code that might cause the compiler to hang during type checking.

   You should only enable the extension when you actually need to relax these
   checks.

   In this file, without this extension, we would get the following error:

   src/GHCExts/OpenTypeFamilies.hs:40:15: error:
       • Illegal nested type family application ‘AppendSymbol
                                                   (AppendSymbol (NamedType a) " -> ") (NamedType b)’
         (Use UndecidableInstances to permit this)
       • In the type instance declaration for ‘NamedType’
      |
   40 | type instance NamedType (a -> b) =
      |               ^^^^^^^^^
   type instance NamedType (a -> b) =
     NamedType a `AppendSymbol` " -> " `AppendSymbol` NamedType b
-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeOperators #-} -- allows using `AppendSymbol`

{- The following 2 extensions work together: -}

{- Allows using explicit universal quantification `forall a`. Using an explicit
   `forall` brings the type variables into scope in the body of the function.
-}
{-# LANGUAGE ScopedTypeVariables #-}

{- Gives you the ability to pass type names as arguments to polymorphic
   functions, to select the type class instance that's used.

   read @Integer "1"
-}
{-# LANGUAGE TypeApplications #-}

{-
src/GHCExts/OpenTypeFamilies.hs:94:17: error:
   • Non type-variable argument
       in the constraint: KnownSymbol (NamedType t)
   • In the type signature:
       showTypeName :: forall t. (KnownSymbol (NamedType t)) => String
   Suggested fix: Perhaps you intended to use FlexibleContexts
  |
94 | showTypeName :: forall t. (KnownSymbol (NamedType t)) => String
  |                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}
{-# LANGUAGE FlexibleContexts #-}

{-
src/GHCExts/OpenTypeFamilies.hs:107:17: error:
    • Could not deduce (KnownSymbol (NamedType t0))
      from the context: KnownSymbol (NamedType t)
        bound by the type signature for:
                   showTypeName :: forall t. KnownSymbol (NamedType t) => String
        at src/GHCExts/OpenTypeFamilies.hs:107:17-63
      The type variable ‘t0’ is ambiguous
    • In the ambiguity check for ‘showTypeName’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In the type signature:
        showTypeName :: forall t. (KnownSymbol (NamedType t)) => String
    |
107 | showTypeName :: forall t. (KnownSymbol (NamedType t)) => String
    |                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GHCExts.OpenTypeFamilies where
{- This module defines the kinds we'll be working with, and a number of utility
   functions to help us integrate our type level code with term level code.

   symbolVal :: KnownSymbol n => proxy n -> String

   The KnownSymbol constraint is a type class constraint that's defined for all
   Symbol types. This means we know that `n` must be some kind of type level
   string.
-}
import GHC.TypeLits
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

   src/GHCExts/OpenTypeFamilies.hs:58:1: error:
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
