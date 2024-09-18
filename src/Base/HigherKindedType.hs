{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

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

