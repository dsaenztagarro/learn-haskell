{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
module Base.Deriving.AnyclassDeriving where

class Redacted a where
  redacted :: a -> String
  {-
  `DefaultSignatures` extension allow us to add a type signature to the default
  implementation of a function in a type class.
  -}
  default redacted :: Show a => a -> String
  redacted = show

{-
`DeriveAnyClass` extension allows us to derive any class that we want, and GHC
will add an empty instance declaration for us.
-}
newtype UserName = UserName String deriving (Show, Redacted)

