{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/roles.html#roles
module Base.Roles where

{-
Using `GeneralizedNewtypeDeriving` (Generalised derived instances for newtypes),
a programmer can take existing instances of classes and “lift” these into
instances of that class for a newtype. However, this is not always safe.
-}

newtype Age = MkAge { unAge :: Int }

-- with -XTypeFamilies
type family Inspect x
type instance Inspect Age = Int
type instance Inspect Int = Bool

class BadIdea a where
  bad :: a -> Inspect a

instance BadIdea Int where
  bad = (> 0)

-- with -XGeneralizedNewtypeDeriving -XStandaloneDeriving
-- not allowed!
-- deriving instance BadIdea Age

{-
If the derived instance were allowed, what would the type of its method `bad` be?
It would seem to be `Age -> Inspect Age`, which is equivalent to `Age -> Int`,
according to the type family `Inspect`.
Yet, if we simply adapt the implementation from the instance for `Int`, the
implementation for bad produces a `Bool`, and we have trouble.

The way to identify such situations is to have roles assigned to type variables
of datatypes, classes, and type synonyms.

Roles as implemented in GHC are a from a simplified version of the work
described in Generative type abstraction and type-level computation.

See docs/generative-type-abstraction-and-type-level-computation.pdf
-}

{-
Role Annotations

Sometimes the programmer wants to constrain the inference process. For example,
we can consider a type `Set a` that represents a set of data, ordered according
to `a`’s `Ord` instance. While it would generally be type-safe to consider a to
be at role representational, it is possible that a newtype and its base type
have different orderings encoded in their respective `Ord` instances. This would
lead to misbehavior at runtime. So, the author of the `Set` datatype would like
its parameter to be at role nominal. This would be done with a declaration.
-}

--
-- newtype MySet = MySet { getSet :: Set Double } deriving newtype (Show,Eq,Ord,Num)

{-
ERROR will happen using "nominal" role
    • Couldn't match type ‘NotInt’ with ‘Int’
        arising from the coercion of the method ‘+’
          from type ‘Set Int -> Set Int -> Set Int’
            to type ‘MySet -> MySet -> MySet’
    • When deriving the instance for (Num MySet)
   |
82 |   deriving Num via Set Int
   |            ^^^
-}
type role Set representational
data Set a = MkSet a deriving (Show,Eq,Ord)

instance Num a => Num (Set a) where
  (MkSet a) + (MkSet b) = MkSet (a + b)
  (MkSet a) * (MkSet b) = MkSet (a * b)
  abs (MkSet a) = MkSet (abs a)
  signum (MkSet a) = MkSet (signum a)
  negate (MkSet a) = MkSet (negate a)

newtype NotInt = NotInt Int
  deriving (Show,Eq,Ord)

newtype MySet = MySet { getSet :: Set NotInt }
  deriving (Show,Eq,Ord)
  deriving Num via Set Int

{-
Role annotations can also be used should a programmer wish to write a class with
a representational (or phantom) role. However, as a class with non-nominal roles
can quickly lead to class instance incoherence, it is necessary to also specify
`IncoherentInstances` to allow non-nominal roles for classes.

Role annotations are allowed on data, newtype, and class declarations. A role
annotation declaration starts with `type role` and is followed by one role
listing for each parameter of the type. (This parameter count includes
parameters implicitly specified by a kind signature in a GADT-style data or
newtype declaration.)

Each role listing is a role (`nominal`, `representational`, or `phantom`) or a
`_`. Using a `_` says that GHC should infer that role. The role annotation may
go anywhere in the same module as the datatype or class definition (much like a
value-level type signature). Here are some examples:
-}

type role T1 _ phantom
data T1 a b = MkT1 a     -- b is not used; annotation is fine but unnecessary

{-
type role T2 _ phantom
data T2 a b = MkT2 b     -- ERROR: b is used and cannot be phantom
-}

type role T3 _ nominal
data T3 a b = MkT3 a     -- OK: nominal is higher than necessary, but safe

type role T4 nominal
data T4 a = MkT4 (a Int) -- OK, but nominal is higher than necessary

type role C representational _   -- with -XIncoherentInstances
class C a b where     -- OK, b will get a nominal role
                      -- with -XMultiParamTypeClasses -XAllowAmbiguousTypes
  id :: a -> a

{-
type role X nominal
type X a = Maybe a     -- ERROR: role annotations not allowed for type synonyms
-}
