-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html
{-# LANGUAGE ExistentialQuantification #-}

module GHCExt.ExistentitallyQuantifiedDataConstructors where

data Foo = forall a. MkFoo a (a -> Bool)
         | Nil

{-
  The data type Foo has two constructors with types:

  MkFoo :: forall a. a -> (a -> Bool) -> Foo
  Nil   :: Foo

  Notice that the type variable `a` in the type of `MkFoo` does not appear in
  the data type itself, which is plain `Foo`.

  [MkFoo 3 even, MkFoo 'c' isUpper] :: [Foo]

  Here, `(MkFoo 3 even)` packages an integer with a function `even` that maps
  an integer to `Bool`; and `MkFoo 'c' isUpper` packages a character with a
  compatible function. These two things are each of type `Foo` and can be put
  in a list.

  Since all we know about `val` and `fn` is that they are compatible, the only
  (useful) thing we can do with them is to apply `fn` to val to get a boolean.
-}

f :: Foo -> Bool
f (MkFoo val fn) = fn val
f Nil = False

{-
  What this allows us to do is to package heterogeneous values together with a
  bunch of functions that manipulate them, and then treat that collection of
  packages in a uniform manner. You can express quite a bit of
  object-oriented-like programming this way.
-}

-- Existentials and type classes
-- -----------------------------

data Baz = forall a. Eq a => Baz1 a a
         | forall b. Show b => Baz2 b (b -> b)

{-
  The two constructors have the types you’d expect:

  Baz1 :: forall a. Eq a => a -> a -> Baz
  Baz2 :: forall b. Show b => b -> (b -> b) -> Baz

  But when pattern matching on `Baz1` the matched values can be compared for
  equality, and when pattern matching on `Baz2` the first matched value can be
  converted to a string (as well as applying the function to it).
-}

g :: Baz -> String
g (Baz1 p q) | p == q    = "Yes"
             | otherwise = "No"
g (Baz2 v fn)            = show (fn v)

-- Record Constructors
-- -------------------

-- GHC allows existentials to be used with records syntax as well.

data Counter a = forall self. NewCounter
  { _this :: self
  , _inc :: self -> self
  , _render :: self -> String
  , tag :: a
  }

{-
  Here `tag` is a public field, with a well-typed selector function
  `tag :: Counter a -> a`.

  The self type is hidden from the outside; any attempt to apply _this, _inc or
  _display as functions will raise a compile-time error. In other words, GHC
  defines a record selector function only for fields whose type does not
  mention the existentially-quantified variables. (This example used an
  underscore in the fields for which record selectors will not be defined, but
  that is only programming style; GHC ignores them.)

  To make use of these hidden fields, we need to create some helper functions:
-}

inc :: Counter a -> Counter a
inc (NewCounter x i r t) = NewCounter
  { _this = i x, _inc = i, _render = r, tag = t }

render :: Counter a -> String
render NewCounter { _this = x, _render = r } = r x

-- Now we can define counters with different underlying implementations:

counterA :: Counter String
counterA = NewCounter
  { _this = 0 :: Int, _inc = (1+), _render = show, tag = "A" }

counterB :: Counter String
counterB = NewCounter
  { _this = "", _inc = ('#':), _render = show, tag = "B" }
