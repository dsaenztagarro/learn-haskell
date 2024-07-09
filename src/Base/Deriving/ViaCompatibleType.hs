{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}

module Base.Deriving.ViaCompatibleType where

import Numeric
import Data.Kind (Type)

newtype Hex a = Hex a

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex a) = "0x" ++ showHex a ""

newtype Unicode = U Int
  deriving Show
    via (Hex Int)

-- With Higher Kinded Types

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
