-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Exts.TypeSignatures.ScopedTypeVariables
-- Stage       : 04-TypeSignatures  (see docs/ROADMAP.md)
-- Source      : GHC:scoped-type-variables
--               https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/scoped_type_variables.html
--
-- == Concept
-- A type variable bound by an explicit @forall@ in a function's
-- signature is brought into scope inside the function's body. Inner
-- @where@/@let@ signatures can then reference the same @a@ instead of
-- introducing a fresh one.
module Exts.TypeSignatures.ScopedTypeVariables where

-- Declaration type signatures

f :: forall a. [a] -> [a]
f xs = ys ++ ys
     where
       ys :: [a] -- <<
       ys = reverse xs

-- Class and instance declarations

class C a where
  op :: [a] -> a

  op xs = let ys::[a]
              ys = reverse xs
          in
          head ys

instance C b => C [b] where
  op xs = reverse (head (xs :: [[b]]))

{-
Alternatively, one could write the instance above as:

instance forall b. C b => C [b] where
  op xs = reverse (head (xs :: [[b]]))

-}
