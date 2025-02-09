{-# LANGUAGE OverloadedStrings #-}

{-
When we enable it, types for String literals become more generic, as follows:

:t "hello"
"hello" :: String
:set -XOverloadedStrings
:t "hello"
"hello" :: forall {a}. Data.String.IsString a => a

The IsString type class defines only one method, `fromString`, as shown next:

fromString :: IsString a => String -> a

The only thing the extension `OverloadedStrings` is responsible for is replacing
every string literal in the source code with a call to the `fromString` method
on that literal. Then it's time for instance resolution algorithms to find the
right instance and convert a `String` to some other type. Note it should be
unambiguous from the context which type is expected at the position of a `String`
literal.

To disable the extension:
:set -XNoOverloadedStrings
-}

module LanguageExtensions.OverloadedStrings where

data Person = Person String (Maybe Int)

instance IsString Person where
  fromString name = Person name Nothing

spj :: Person
spj = "Simon Peyton Jones"
