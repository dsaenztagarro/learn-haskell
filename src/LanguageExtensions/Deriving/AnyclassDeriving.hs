{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
module LanguageExtensions.Deriving.AnyclassDeriving where

{-
Haskell 98 allows you to define a default implementation when declaring a class:

class Redacted a where
  redacted :: a -> String
  redacted = "Untitled"

The type of the `redacted` method is `a -> String`, and this is also the type of
the default method. You can lift this restriction and give another type to the
default method using the flag -XDefaultSignatures.
-}

class Redacted a where
  redacted :: a -> String
  default redacted :: Show a => a -> String
  redacted = show

{-
`DeriveAnyClass` extension allows us to derive any class that we want, and GHC
will add an empty instance declaration for us.
-}
newtype UserName = UserName String deriving (Show, Redacted)

