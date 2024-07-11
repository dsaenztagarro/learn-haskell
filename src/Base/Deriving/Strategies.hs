{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Base.Deriving.Strategies where

class Redacted a where
  redacted :: a -> String
  default redacted :: Show a => a -> String
  redacted = show

class Cool a where
  cool :: String
  default cool :: String
  cool = "Cool!"

newtype UserName = UserName String deriving Show
instance Redacted UserName where
  redacted (UserName user) = "UserName: " <> user

newtype AdminUser = AdminUser UserName
  deriving stock Show
  deriving newtype Redacted

{-
With the `DerivingStrategies` extension we can add several deriving clauses when
we define a type, and each of those can have an associated deriving strategy.
There are four strategies you can use:
- `stock` can be used if you are trying to derive one of the standard derivable
  type classes and want to use the standard approach defined in the Haskell
  standard.
- `newtype` tells the compiler to use the generalized newtype deriving strategy
  to create an instance based on a newtype wrapper.
- `anyclass` tells the compiler to generate and empty instance declaration,
  which is the default when DeriveAnyClass is enabled.
- `via` uses the deriving via strategy.
-}
