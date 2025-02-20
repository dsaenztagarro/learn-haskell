-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables
{-# LANGUAGE ScopedTypeVariables #-}
module LanguageExtensions.TypeSignatures.ScopedTypeVariables where

-- Declaration type signatures

f :: forall a. [a] -> [a]
f xs = ys ++ ys
     where
       ys :: [a]
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
