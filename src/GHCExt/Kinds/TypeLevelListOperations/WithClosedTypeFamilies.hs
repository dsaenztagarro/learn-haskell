{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-
    src/GHCExt.Kinds.TypeLevelListOperations.hs:10:31: error:
        Unexpected kind variable ‘a’
        Perhaps you intended to use PolyKinds
        In the declaration for type family ‘Member’
       |
    10 | type family Member (needle :: a) (haysstack :: [a]) :: Bool where
       |                               ^

    src/GHCExt.Kinds.TypeLevelListOperations.hs:10:49: error:
        Unexpected kind variable ‘a’
        Perhaps you intended to use PolyKinds
        In the declaration for type family ‘Member’
       |
    10 | type family Member (needle :: a) (haysstack :: [a]) :: Bool where
       |                                                 ^
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- Type Level List Operations with Type Families
module GHCExt.Kinds.TypeLevelListOperations.WithClosedTypeFamilies where

import GHC.TypeLits
import Data.Kind

{- Closed type family to see if a particular value is inside a type level list

   At the type level using the same variable in a pattern binded to two
   different expresions means that the pattern will only match if the value of
   the variable is equal in both places.
-}
type family Member (needle :: a) (haysstack :: [a]) :: Bool where
  Member a '[] = False
  Member a (a : as) = True
  Member a (b : as) = Member a as -- nothings stops `a` and `b` from being equal

type family IfThenElse (p :: Bool) (t :: a) (f :: a) where
  IfThenElse True t _ = t
  IfThenElse False _ f = f

{- FindElems needs to take a function. We can't pass a type family in, because
   we're not allowed to use type families that haven't had all of their
   arguments applied, but type families aren't the only way we can represent
   something like (a -> Bool) at the type level.

   Type constructors take arguments, and we are allowed to pass those into a
   type family without having applied all of their arguments.

   In programming languages, defunctionalization is a compile-time
   transformation which eliminates higher-order functions, replacing them by a
   single first-order apply function.
-}

type ReturnValue r = r -> Type
data Even :: Natural -> ReturnValue Bool

type family FindElems (p :: a -> ReturnValue Bool) (elems :: [a]) :: [a] where
  FindElems _ '[] = '[]
  FindElems p (a : as) =
    IfThenElse (EvalEven (p a)) (a : FindElems p as) (FindElems p as)

type family EQ (a :: k) (b :: k) :: Bool where
  EQ a a = True
  EQ a b = False

type family EvalEven (expr :: Bool -> Type) :: Bool where
  EvalEven (Even n) = EQ 0 (n `Mod` 2)
