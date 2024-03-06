{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExistentialDemo2 where

class SomeClass a b where
  modifyClassValue :: a -> a
  combineClassValues :: a -> a -> a
  consumeClassValue :: a -> b

-- To mimic addAndMultiplyInt
instance Integral a => SomeClass a Int where
  modifyClassValue a = a + a
  combineClassValues = (*)
  consumeClassValue = fromIntegral

runSomeClass :: forall a b. SomeClass a b => a -> b
runSomeClass val =
  let
    modified = modifyClassValue @a @b val
    combined = combineClassValues @a @b modified val
  in consumeClassValue combined

