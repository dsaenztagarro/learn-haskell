module Std.Data.Either where

import Std.Data.Functor
import Std.Control.Applicative
import Prelude (Show)

data Either a b = Left a | Right b deriving Show

instance Functor (Either a) where
  fmap f (Left a) = Left a
  fmap f (Right a) = Right (f a)

instance Applicative (Either a) where
  pure a = Right a
  (Left err) <*> _ = Left err
  (Right f) <*> g = f <$> g


-- Defining a version of fmap that works on Left values
-- Each type can only have one instance of a type class
-- Create a newtype and define the new instance of the type class with the
-- different behavior

newtype ReverseEither a b = ReverseEither (Either b a) deriving Show

instance Functor (ReverseEither a) where
  fmap f (ReverseEither (Left a)) = ReverseEither (Left (f a))
  fmap f (ReverseEither (Right a)) = ReverseEither (Right a)

