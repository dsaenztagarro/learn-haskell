{-# LANGUAGE NoImplicitPrelude #-}
module StandardLibrary.Data.Maybe where

import StandardLibrary.Data.Functor
import StandardLibrary.Control.Applicative
import StandardLibrary.Control.Monad
import Prelude (Int, (>=), (<=), (&&), even)

data Maybe a = Nothing | Just a

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just f <*> a = f <$> a

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just a >>= f = f a

-- TODO: move to tests examples

half :: Int -> Maybe Int
half num =
  if even num
  then Just num
  else Nothing

bound :: (Int, Int) -> Int -> Maybe Int
bound (min, max) num =
  if (num >= min) && (num <= max)
  then Just num
  else Nothing

-- Text.readMaybe "11" >>= bound (0, 20) >>= return . succ >>= half
-- Just 6
