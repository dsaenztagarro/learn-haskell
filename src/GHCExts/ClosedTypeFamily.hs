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

module GHCExts.ClosedTypeFamily where
import GHC.TypeLits

data Peano = Zero | Succ Peano

{- Overlap is allowed in closed families. Like guard clauses and case
   expresions, overlap in a closed type family is resolved top-to-bottom.

   `ToPeano` converts a type-level natural number to its Peano representation
-}
type family ToPeano (n :: Natural) :: Peano where
  ToPeano 0 = Zero
  ToPeano a = Succ (ToPeano (a - 1))

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
