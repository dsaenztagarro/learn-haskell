{-# LANGUAGE NoImplicitPrelude #-}
module Std.Data.List where

import Std.Data.Functor
import Std.Control.Applicative
import Std.Control.Monad
import Prelude (show, (.), Show)

data List a = Empty | List a (List a)

instance Show a => Show (List a) where
  show = show . fromList

instance Functor List where
  fmap _ Empty = Empty
  fmap f (List a as) = List (f a) (fmap f as)

instance Applicative List where
  pure a = List a Empty
  Empty <*> _ = Empty
  List f fs <*> vals = (f <$> vals) `concatList` (fs <*> vals)

instance Monad List where
  return = pure
  Empty >>= _f = Empty
  List a as >>= f = (f a) `concatList` (as >>= f)

toList :: [a] -> List a
toList [] = Empty
toList (a:as) = List a (toList as)

fromList :: List a -> [a]
fromList Empty = []
fromList (List a as) = a : fromList as

concatList :: List a -> List a -> List a
concatList Empty as = as
concatList (List a as) bs = List a (concatList as bs)
