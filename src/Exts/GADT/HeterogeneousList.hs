{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Exts.GADT.HeterogeneousList
-- Stage       : 05-Existentials  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — GADT chapter
-- Prereqs     : Exts.Types.ExistentialQuantification
--
-- == Concept
-- Unlike an existential-based list (where every element has the same
-- erased type), this GADT-based list /preserves/ the type of every
-- element in the list's type parameter — a type-level list. The
-- @:++:@ constructor cons-cells both at the value and the type level.
--
-- == Example
-- >>> let xs = UserByName \"Alice\" \"X\" :++: UserByID 1 :++: EmptyUsers
-- >>> length (nameUsers xs)
-- 1
module Exts.GADT.HeterogeneousList where

data User a where
  UserByName ::
    { userFirst :: String, userLast :: String } -> User String
  UserByID ::
    { userID :: Int } -> User Int

-- StandaloneDeriving
deriving instance Eq a => Eq (User a)
deriving instance Show a => Show (User a)

usersWithFirstName :: String -> [User String] -> [User String]
usersWithFirstName firstName = filter ((== firstName) . userFirst)

{- Unlike existential-based heterogeneous lists, our GADT approach retains the
   detailed type information for every element in the list.
   Our list has a type parameter that is itself a list of the type of every
   element that has been inserted into the list.
   Each time we add a new value, we also add its type to the type of the list.
-}
infixr 9 :++:
data Heterogeneous a where
  EmptyUsers :: Heterogeneous '[]
  (:++:) :: User a -> Heterogeneous as -> Heterogeneous (a : as)

-- Extracting fields from a Heterogeneous List
nameUsers :: Heterogeneous a -> [User String]
nameUsers EmptyUsers = []
nameUsers (user :++: users) =
  case user of
    UserByName _ _ -> user : nameUsers users
    UserByID _ -> nameUsers users

