{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LanguageExtensions.TypeClasses.FlexibleContexts  where

{-
Allow the use of complex constraints in class declaration contexts.

In Haskell 98 the context of a class declaration (which introduces superclasses)
must be simple; that is, each predicate must consist of a class applied to type
variables.

The extension FlexibleContexts lifts this restriction, so that the only
restriction on the context in a class declaration is that the class hierarchy
must be acyclic. So these class declarations are OK:
-}

class Functor (m k) => FiniteMap m k where

class (Monad m, Monad (t m)) => Transform t m where
  lift :: m a -> (t m) a

{-
As in Haskell 98, the class hierarchy must be acyclic. However, the definition
of “acyclic” involves only the superclass relationships. For example, this is okay:
-}

class C a where
  op :: D b => a -> b -> b

class C a => D a where

{-
Here, `C` is a superclass of `D`, but it’s OK for a class operation `op` of `C`
to mention `D`. (It would not be OK for `D` to be a superclass of `C`.)
-}
