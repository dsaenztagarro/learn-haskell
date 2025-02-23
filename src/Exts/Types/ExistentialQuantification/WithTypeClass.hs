{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

-- WARNING: This is NOT ExistentialQuantification
-- Just simulating existential quantification by only using Type classes

module Exts.Types.ExistentialQuantification.WithTypeClass where

{-
Since neither `modifyClassValue` nor `combineClassValue` ever refer to b; they
are ambiguous (i.e. if we call them the compiler doesn't know which instance it
should use, and so without AllowAmbiguousTypes extension, it will not try at all
and will give us an error.
-}

class SomeClass a b where
  modifyClassValue :: a -> a
  combineClassValues :: a -> a -> a
  consumeClassValue :: a -> b

{-
ScopedTypeVariables and TypeApplications are enabled so we can tell GHC exactly
what types we want to use when we call `modifyClassValue` nor `combineClassValue`.

We have to do this for the same reason that we had to enable AllowAmbiguousTypes
earlier when defining our class: since these types never refer to be explicitly,
without a type annotation GHC can't figure out what versions of the function to
call.
-}

runSomeClass :: forall a b. SomeClass a b => a -> b
runSomeClass val =
  let modified = modifyClassValue @a @b val
      combined = combineClassValues @a @b modified val
  in consumeClassValue combined

instance Integral a => SomeClass a Int where
  modifyClassValue a = a + a
  combineClassValues = (*)
  consumeClassValue = fromIntegral
