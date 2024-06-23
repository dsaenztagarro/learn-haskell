{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module GHCExt.Kinds.TypeLevelListOperations.WithOpenTypeFamilies where

import GHC.TypeLits
import Data.Kind

type family IfThenElse (p :: Bool) (t :: a) (f :: a) where
  IfThenElse True t _ = t
  IfThenElse False _ f = f

type family EQ (a :: k) (b :: k) :: Bool where
  EQ a a = True
  EQ a b = False

type ReturnValue r = r -> Type
data LessThanOrEqual :: Natural -> Natural -> ReturnValue Bool
data Even :: Natural -> ReturnValue Bool

type family Eval (expr :: ReturnValue r) :: r
type instance Eval (LessThanOrEqual a b) = b <=? a
type instance Eval (Even n) = EQ 0 (n `Mod` 2)

type family FindElems (p :: a -> ReturnValue Bool) (elems :: [a]) :: [a] where
  FindElems _ '[] = '[]
  FindElems p (a : as) =
    IfThenElse (Eval (p a)) (a : FindElems p as) (FindElems p as)
