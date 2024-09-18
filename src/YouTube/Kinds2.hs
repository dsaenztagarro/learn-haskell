-- Tweag: @rae: Getting a little fancy with Haskell's kinds
-- https://www.youtube.com/watch?v=Qy_yxVkO8no

{-# LANGUAGE StandaloneKindSignatures, ConstraintKinds, FunctionalDependencies,
             FlexibleInstances #-}
module YouTube.Kinds2 where

import Data.Kind
import qualified Data.Set as Set

-- Example: building Functor abstraction for any container (Set, List,..)

s1 :: Set.Set Int
s1 = Set.fromList [2,4,2,3]

s2 :: Set.Set Int
s2 = Set.map (+1) s1

s3 :: Set.Set Int
s3 = fmapC (+1) s1

type FunctorC :: (Type -> Constraint) -> (Type -> Constraint) -> (Type -> Type) -> Constraint
class FunctorC c1 c2 f | f -> c1 c2 where
  fmapC :: (c1 a, c2 b) => (a -> b) -> f a -> f b

{-
:t Set.map
Set.map :: Ord b => (a -> b) -> Set.Set a -> Set.Set b
-}

type Always :: Type -> Constraint
class Always a
instance Always a

instance FunctorC Always Ord Set.Set where
  fmapC = Set.map
