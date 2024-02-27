{-# LANGUAGE NoImplicitPrelude #-}

module MyLib where
-- module MyLib (someFunc) where

import Prelude (const, Int, even, succ)
import qualified Text.Read as Text

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) a fb = fmap (const a) fb

infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- Functor Laws
-- Identity
-- fmap id f = id f
-- Composition
-- fmap (f . g) = fmap f . fmap g

class Functor f => Applicative f where
  pure :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b

-- Applicative Laws
-- Identity
-- pure id <*> v = v
-- Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- Homomorphism
-- pure f <*> pure x = pure (f x)
-- Interchange
-- u <*> pure y = pure ($ y) <*> u

class Applicative m => Monad m where
  infixl 1 >>=
  (>>=) :: m a -> (a -> m b) -> m b
  infixl 1 >>
  (>>) :: m a -> m b -> m b
  a >> b = a >>= \_ -> b
  return :: a -> m a

-- Monad Laws
-- Left identity
-- return a >>= m = m a
-- Right identity
-- m >>= return = m
-- Associativity
-- (a >>= b) >>= c = a >>= (\x -> b x >>= c)

--------------------------------------

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

Text.readMaybe "11" >>= bound (0, 20) >>= return . succ >>= half
-- Just 6

--------------------------------------

data Either a b = Left a | Right b

instance Functor (Either a) where
  -- fmap :: (b -> c) -> (Either a) b -> (Either a) c
  fmap f (Left a) = Left a
  fmap f (Right a) = Right (f a)

instance Applicative (Either a) where
  pure a = Right a
  (Left err) <*> _ = Left err
  (Right f) <*> g = f <$> g

--------------------------------------

data List a = Empty | List a (List a)

toList :: [a] -> List a
toList [] = Empty
toList (a:as) = List a (toList as)

fromList :: List a -> [a]
fromList Empty = []
fromList (List a as) = a : fromList as

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

instance Monad List where
  return a = List a Empty
  Empty >>= f = Empty
  List a as >>= f = (f a) `concatList` (as >>= f)

instance Show a => Show (List a) where
  show = show . fromList

--------------------------------------

newtype Function a b = Function { runFunction :: a -> b }

instance Functor (Function a) where
  fmap f (Function g) = Function (f . g)

instance Applicative (Function a) where
  pure a = Function $ const a
  -- (<*>) :: Function a (b -> c) -> Function a b -> Function a c
  Function f <*> Function g = Function $ \value -> f value (g value)
