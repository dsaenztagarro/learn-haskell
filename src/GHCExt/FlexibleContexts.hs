{-
The `FlexibleContexts` extension lifts the Haskell 98 restriction that the
type-class constraints (anywhere they appear) must have the form
(class type-variable) or (class (type-variable type1 type2 … typen)).
With `FlexibleContexts` these type signatures are perfectly okay:

g :: Eq [a] => ...
g :: Ord (T a ()) => ...

This extension does not affect equality constraints in an instance context;
they are permitted by `TypeFamilies` or `GADTs`.

Note that `FlexibleContexts` affects usages of class constraints, in type
signatures and other contexts. In contrast, `FlexibleInstances` loosens a
similar restriction in place when declaring a new instance.
-}
{-# LANGUAGE FlexibleContexts #-}
module GHCExt.FlexibleContexts  where

g :: Eq [a] => a -> a
g = undefined
