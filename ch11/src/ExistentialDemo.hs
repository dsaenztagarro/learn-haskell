{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module ExistentialDemo where

-- Existential records can be useful in cases where you want something like a
-- lightweight and ad hoc type class, or when you need some additional constant
-- values associated with your functions without the overhead of defining
-- multiple type classes.
data SomeExistential b = forall a. SomeExistential
  { someValue :: a
  , modifyValue :: a -> a
  , combineValues :: a -> a -> a
  , consumeValue :: a -> b
  }

instance Functor SomeExistential where
  fmap = modifyExistential

runExistential :: SomeExistential a -> a
runExistential SomeExistential{..} =
  consumeValue $
    combineValues (modifyValue someValue) someValue

runSimple :: SomeExistential a -> a
runSimple SomeExistential{..} = consumeValue someValue

modifyExistential :: (a -> b) -> SomeExistential a -> SomeExistential b
modifyExistential f SomeExistential{..} = SomeExistential
  { someValue = someValue
  , modifyValue = modifyValue
  , combineValues = combineValues
  , consumeValue = f . consumeValue
  }

-- runExistential $ show <$> addAndMultiplyInt 7
-- "98"
addAndMultiplyInt :: Integral a => a -> SomeExistential Int
addAndMultiplyInt n = SomeExistential
  { someValue = n
  , modifyValue = (+n)
  , combineValues = (*)
  , consumeValue = fromIntegral
  }

-- runExistential $ length <$> reverseAndUnwordsString "hello"
-- 11
reverseAndUnwordsString :: String -> SomeExistential String
reverseAndUnwordsString s = SomeExistential
  { someValue = s
  , modifyValue = reverse
  , combineValues = \a b -> unwords [a,b]
  , consumeValue = id
  }

-- runExistential $ constExistential 7
-- 7
constExistential :: Int -> SomeExistential Int
constExistential n = SomeExistential
  { someValue = n
  , modifyValue = const n
  , combineValues = const $ const n
  , consumeValue = const n
  }

