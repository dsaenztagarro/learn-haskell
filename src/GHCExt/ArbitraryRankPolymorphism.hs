{-# LANGUAGE RankNTypes #-}

module GHCExt.ArbitraryRankPolymorphism where

-- With `RankNTypes` You can nest foralls arbitrarily deep in function arrows.

-- GHC’s type system supports arbitrary-rank explicit universal quantification
-- in types. For example, all the following types are legal:

-- Here, f1 and g1 are rank-1 types, and can be written in standard Haskell
-- (e.g. f1 :: a->b->a). The forall makes explicit the universal quantification
-- that is implicitly added by Haskell.
f1 :: forall a b. a -> b -> a
f1 a _b = a
g1 :: forall a b. (Ord a, Eq  b) => a -> b -> a
g1 a _b = a

-- The functions f2 and g2 have rank-2 types; the forall is on the left of a
-- function arrow. As g2 shows, the polymorphic type on the left of the function arrow can be overloaded.
f2 :: (forall a. a->a) -> Int -> Int
f2 _f a = a
g2 :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int
g2 _f a = a

-- The function f3 has a rank-3 type; it has rank-2 types on the left of a
-- function arrow.
f3 :: ((forall a. a->a) -> Int) -> Bool -> Bool
f3 _f b = b


-- The function h1 has a rank-1 type; it has the same behaviour as h1', except
-- with a different order of arguments. This matters if one were to specify the
-- type explicitly using a visible type application (using TypeApplications):
-- we would write `h1 3 @Bool True` but `h1' @Bool 3 True`.
h1 :: Int -> (forall a. a -> a)
h1 _a = id
h1' :: forall a. Int -> (a -> a)
h1' _a = id
