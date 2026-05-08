{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

-- |
-- Module      : Base.HigherKindedType
-- Stage       : 02-Base  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Kinds chapter
-- Prereqs     : Std.Data.Functor (familiarity with @Functor f@ where @f :: * -> *@)
--
-- == Concept
-- A /higher-kinded type/ is a type constructor that itself takes a type
-- constructor as a parameter. Here, @toCSV@ is generic over any
-- @t :: * -> *@ that is 'Foldable'. The explicit kind signatures make the
-- /kind/ of each type variable visible at the source level.
--
-- == Example
-- >>> toCSV [1, 2, 3 :: Int]
-- "1,2,3"
-- >>> toCSV (Just "x")
-- "\"x\""
--
-- == Exercise
-- Generalise @toCSV@ to take a separator. What constraints (if any) on
-- @t@ change?
module Base.HigherKindedType where
import Data.Kind

-- Type class constraint on t
-- We're applying type `a` to `t` because `t` is a higher kinded type variable,
-- with the kind `t :: * -> *`
toCSV ::
  forall (t :: Type -> Type) (a :: Type) -- << using kind
  . (Foldable t, Show a)
  => t a -> String
toCSV =
  let
    addField :: Show a => String -> a -> String
    addField s a = s <> "," <> show a
  in (drop 1) . foldl addField ""

