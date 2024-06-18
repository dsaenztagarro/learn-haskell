{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module GHCExts.ExistentialTypes.UsingTypeClass where

class SomeClass a b where
  modifyClassValue :: a -> a
  combineClassValues :: a -> a -> a
  consumeClassValue :: a -> b

runSomeClass :: forall a b. SomeClass a b => a -> b
runSomeClass val =
  let modified = modifyClassValue @a @b val
      combined = combineClassValues @a @b modified val
  in consumeClassValue combined

instance Integral a => SomeClass a Int where
  modifyClassValue a = a + a
  combineClassValues = (*)
  consumeClassValue = fromIntegral
