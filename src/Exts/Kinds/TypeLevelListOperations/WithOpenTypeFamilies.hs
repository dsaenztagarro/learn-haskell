{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Exts.Kinds.TypeLevelListOperations.WithOpenTypeFamilies
-- Stage       : 06-Kinds  (see docs/ROADMAP.md)
-- Source      : EH:ch12 type-families
-- Prereqs     : Exts.Kinds.OpenTypeFamily
--
-- == Concept
-- Implements typical list operations (membership, equality, filtering
-- by predicate) at the type level using *open* type families. Each
-- operation is one or more @type instance@ declarations that GHC
-- resolves by structural matching only. Same exercises appear in
-- 'WithClosedTypeFamilies' for direct contrast.
module Exts.Kinds.TypeLevelListOperations.WithOpenTypeFamilies where

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
