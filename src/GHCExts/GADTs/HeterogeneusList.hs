{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module GHCExts.GADTs.HeterogeneusList where

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

{- Unlike existential-based heterogeneus lists, our GADT approach retains the
   detailed type information for every element in the list.
   Our list has a type parameter that is itself a list of the type of every
   element that has been inserted into the list.
   Each time we add a new value, we also add its type to the type of the list.
-}
infixr 9 :++:
data Heterogeneus a where
  EmptyUsers :: Heterogeneus '[]
  (:++:) :: User a -> Heterogeneus as -> Heterogeneus (a : as)

-- Extracting fields from a Heterogeneus List
nameUsers :: Heterogeneus a -> [User String]
nameUsers EmptyUsers = []
nameUsers (user :++: users) =
  case user of
    UserByName _ _ -> user : nameUsers users
    UserByID _ -> nameUsers users

