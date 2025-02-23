-- This extension implies the `ExplicitForAll` feature
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

{-
Existential types give a way to "weaken" a type into a representation that
is more general, but about which you have less information.
-}
module LanguageExtensions.Types.ExistentialQuantification.WithRecord where

{-
What this allows us to do is to package heterogeneous values together with a
bunch of functions that manipulate them, and then treat that collection of
packages in a uniform manner. You can express quite a bit of
object-oriented-like programming this way.
-}

data SomeExistential b = forall a. SomeExistential
  { someValue :: a
  , modifyValue :: a -> a
  , combineValues :: a -> a -> a
  , consumeValue :: a -> b
  }

runExistential :: SomeExistential a -> a
runExistential SomeExistential{..} =
  consumeValue $
    combineValues (modifyValue someValue) someValue

addAndMultiplyInt :: Integral a => a -> SomeExistential Int
addAndMultiplyInt n = SomeExistential
  { someValue = n
  , modifyValue = (+n)
  , combineValues = (*)
  , consumeValue = fromIntegral
  }

reverseAndUnwordsString :: String -> SomeExistential String
reverseAndUnwordsString s = SomeExistential
  { someValue = s
  , modifyValue = reverse
  , combineValues = \a b -> unwords [a,b]
  , consumeValue = id
  }

constExistential :: Int -> SomeExistential Int
constExistential n = SomeExistential
  { someValue = n
  , modifyValue = const n
  , combineValues = const $ const n
  , consumeValue = const n
  }

