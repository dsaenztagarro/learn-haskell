{-# LANGUAGE NoImplicitPrelude #-}
module StandardLibrary.Data.List where

import StandardLibrary.Data.Functor
import StandardLibrary.Control.Applicative

data List a = Empty | List a (List a)

instance Functor List where
  fmap _ Empty = Empty
  fmap f (List a as) = List (f a) (fmap f as)

instance Applicative List where
  pure a = List a Empty
  Empty <*> _ = Empty
  List f fs <*> vals = (f <$> vals) `concatList` (fs <*> vals)

concatList :: List a -> List a -> List a
concatList Empty as = as
concatList (List a as) bs = List a (concatList as bs)
