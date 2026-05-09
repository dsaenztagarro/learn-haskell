-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rank_polymorphism.html
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Exts.Types.RankNTypes
-- Stage       : 04-TypeSignatures  (see docs/ROADMAP.md)
-- Source      : GHC:rank-n-types
-- Prereqs     : Exts.TypeSignatures.ExplicitForAll
--
-- == Concept
-- A rank-2 (or higher) type accepts a /still-polymorphic/ value as an
-- argument: the callee, not the caller, is the one allowed to choose
-- the type. Below, @func@ takes a logger that must work for any
-- 'Show'-able @a@, and uses it at multiple types within one body.
--
-- == Exercise
-- Try refactoring 'func' so that the @forall a@ is on @func@'s top
-- level instead of inside the argument arrow. Why does the function no
-- longer typecheck?
module Exts.Types.RankNTypes where

func :: (forall a. Show a => a -> IO ()) -> IO ()
func log = do
  username <- getUsername
  log @String username

  userCount <- getUserCount
  log @Int userCount

getUsername :: IO String
getUsername = undefined

getUserCount :: IO Int
getUserCount = undefined

-- == Grammar examples
--
-- Rank-1 types — the @forall@ is on the outside, equivalent to standard
-- Haskell polymorphism with the quantifier made explicit.
f1 :: forall a b. a -> b -> a
f1 a _b = a
g1 :: forall a b. (Ord a, Eq b) => a -> b -> a
g1 a _b = a

-- Rank-2 types — the @forall@ sits to the left of a function arrow, so the
-- function /receives/ a polymorphic value rather than producing one. As g2
-- shows, the polymorphic argument can be overloaded.
f2 :: (forall a. a -> a) -> Int -> Int
f2 _f a = a
g2 :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int
g2 _f a = a

-- Rank-3 type — a rank-2 type appears on the left of a function arrow.
f3 :: ((forall a. a -> a) -> Int) -> Bool -> Bool
f3 _f b = b

-- Rank-1 again, but with the @forall@ on the right of an arrow. h1 and h1'
-- have the same runtime behaviour but visible type application differs:
-- @h1 3 \@Bool True@ vs @h1' \@Bool 3 True@.
h1 :: Int -> (forall a. a -> a)
h1 _a = id
h1' :: forall a. Int -> (a -> a)
h1' _a = id
