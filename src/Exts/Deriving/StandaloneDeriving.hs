-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/standalone_deriving.html
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Exts.Deriving.StandaloneDeriving
-- Stage       : 03-Deriving  (see docs/ROADMAP.md)
-- Source      : GHC users guide — StandaloneDeriving
--               https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/standalone_deriving.html
--
-- == Concept
-- A @deriving@ clause does not have to live next to its data declaration.
-- The standalone form takes an explicit context, can be /more specific/
-- than the data type (with 'FlexibleInstances'), and is the only way to
-- derive instances for a foreign data type without orphans-via-newtype.
module Exts.Deriving.StandaloneDeriving where

data Foo a = Bar a | Baz String

deriving instance Eq a => Eq (Foo a)

{-
The syntax is identical to that of an ordinary instance declaration apart from
(a) the keyword `deriving`, and (b) the absence of the `where` part.

However, standalone deriving differs from a `deriving` clause in a number of
important ways:

- The standalone deriving declaration does not need to be in the same module as
the data type declaration. (But be aware of the dangers of orphan instances.

- In most cases, you must supply an explicit context (in the example the context
is `Eq(a)`) exactly as you would in an ordinary instance declaration.
In contrast, in a `deriving` clause attached to a data type declaration, the
context is inferred.

The exception to this rule is that the context of a standalone deriving
declaration can infer its context when a single, extra-wildcards constraint is
used as the context, such as in:

deriving instance _ => Eq (Foo a)

This is essentially the same as if you had written `deriving Eq` after the
declaration for `data Foo a`. Using this feature requires the use of
`PartialTypeSignatures` (Partial Type Signatures).

Unlike a deriving declaration attached to a data declaration, the instance can
be more specific than the data type (assuming you also use `FlexibleInstances`,
Instance termination rules). Consider for example:
-}

data Foo' a = Bar' a | Baz' String

deriving instance Eq a => Eq (Foo' [a])
deriving instance Eq a => Eq (Foo' (Maybe a))

{-
This will generate a derived instance for `(Foo [a])` and `(Foo (Maybe a))`, but
other types such as `(Foo (Int,Bool))` will not be an instance of `Eq`.
-}
