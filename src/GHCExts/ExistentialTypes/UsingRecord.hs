{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module GHCExts.ExistentialTypes.UsingRecord where

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

reverseAndUnwordsString s = SomeExistential
  { someValue = s
  , modifyValue = reverse
  , combineValues = \a b -> unwords [a,b]
  , consumeValue = id
  }
