{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      : Exts.Deriving.ViaCompatibleType
-- Stage       : 03-Deriving  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Deriving strategies chapter
-- Prereqs     : Exts.Deriving.GeneralizedNewtypeDeriving, Base.Roles
--
-- == Concept
-- @DerivingVia@ generalises @GeneralizedNewtypeDeriving@: derive an
-- instance for type @T@ by routing through any other type @V@ that has
-- the right representation and the desired instance. Two examples here:
--   * @Unicode@ shows as hex, by deriving 'Show' via @Hex Int@.
--   * @MyMaybe@ borrows 'Semigroup'/'Monoid' from @Sel Maybe a@.
--
-- == Example
-- >>> show (U 65)
-- "0x41"
module Exts.Deriving.ViaCompatibleType where

import Numeric
import Data.Kind (Type)

newtype Hex a = Hex a

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex a) = "0x" ++ showHex a ""

newtype Unicode = U Int
  deriving Show
    via (Hex Int)

-- With Higher Kinded Types

{-
In GHC, two types that have the same underlying runtime representation are
considered representationally equal. When two types are representationally
equal, the compiler can figure out how to safely coerce values from one type to
the other.

Both `Sel Maybe a` and `MyMaybe a` are representationally equal to `Maybe a`,
which means that the compiler knows how to convert back and forth between
`Sel Maybe a` and `MyMaybe a`.

`deriving via` alloww us to derive a type class instance for one type using the
instance defined for another type that is representationally equal.
-}

class Select (f :: Type -> Type) where
  empty :: f a
  pick :: f a -> f a -> f a

instance Select Maybe where
  empty = Nothing
  pick Nothing a = a
  pick a _ = a

newtype Sel (f :: Type -> Type) (a :: Type) = Sel (f a)

instance Select f => Semigroup (Sel f a) where
  (Sel a) <> (Sel b) = Sel (pick a b)

instance Select f => Monoid (Sel f a) where
  mempty = Sel empty

newtype MyMaybe a = MyMaybe (Maybe a)
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via (Sel Maybe a)
