{-# LANGUAGE FlexibleInstances #-}

module LanguageExtensions.TypeClasses.FlexibleInstances where

{-
The FlexibleInstances extension allows the head of the instance declaration to
mention arbitrary nested types.
-}

class C a where

instance C (Maybe Int) where
