{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-
src/GHCExts/ClosedTypeFamily.hs:15:3: error:
    • Illegal nested type family application ‘ToPeano (a - 1)’
      (Use UndecidableInstances to permit this)
    • In the equations for closed type family ‘ToPeano’
      In the type family declaration for ‘ToPeano’
   |
15 |   ToPeano a = Succ (ToPeano (a - 1))
   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
src/GHCExts/ClosedTypeFamily.hs:55:18: error:
    • Non type-variable argument
        in the constraint: KnownSymbol (NamedPeano t)
    • In the type signature:
        showPeanoName :: forall t. (KnownSymbol (NamedPeano t)) => String
    Suggested fix: Perhaps you intended to use FlexibleContexts
   |
55 | showPeanoName :: forall t. (KnownSymbol (NamedPeano t)) => String
   |                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module GHCExts.ClosedTypeFamily where

import GHC.TypeLits
import Data.Proxy

data Peano = Zero | Succ Peano

{- Overlap is allowed in closed families. Like guard clauses and case
   expresions, overlap in a closed type family is resolved top-to-bottom.

   `ToPeano` converts a type-level natural number to its Peano representation
-}
type family ToPeano (n :: Natural) :: Peano where
  ToPeano 0 = Zero
  ToPeano a = Succ (ToPeano (a - 1))

type family FromPeano (a :: Peano) :: Natural where
  FromPeano Zero = 0
  FromPeano (Succ a) = 1 + FromPeano a

type family Add (a :: Peano) (b :: Peano) :: Peano where
  Add a Zero = a
  Add a (Succ b) = Add (Succ a) b

type family Multiply (a :: Peano) (b :: Peano) :: Peano where
  Multiply a Zero = Zero
  Multiply a (Succ Zero) = a
  Multiply a (Succ b) = Add a (Multiply a b)

type family Substract (a :: Peano) (b :: Peano) :: Peano where
  Substract a Zero = a
  Substract (Succ a) (Succ b) = Substract a b

type (:++:) a b = a `AppendSymbol` b

type family NamedPeano (a :: Peano) :: Symbol where
  NamedPeano Zero = "Zero"
  NamedPeano (Succ a) = "(Succ " :++: (NamedPeano a) :++: ")"

showPeanoName :: forall t. (KnownSymbol (NamedPeano t)) => String
showPeanoName = symbolVal $ Proxy @(NamedPeano t)

-- Test Helpers

-- Type-level equality check for testing purpose
type family Equals (a :: Peano) (b :: Peano) :: Bool where
  Equals Zero Zero = 'True
  Equals (Succ a) (Succ b) = Equals a b
  Equals _ _ = 'False

-- Newtype for Type-level assertion is defined with a dummy field
newtype AssertEqual (a :: Peano) (b :: Peano) = AssertEqual ()

-- The `assertEqual` function provides a way to create an `AssertEqual` value,
-- ensuring the equality constraint `(Equals a b ~ 'True)` is satisfied.
assertEqual :: forall a b. (Equals a b ~ 'True) => AssertEqual a b
assertEqual = AssertEqual ()
