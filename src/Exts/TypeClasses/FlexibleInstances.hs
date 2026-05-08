{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Exts.TypeClasses.FlexibleInstances
-- Stage       : 04-TypeSignatures  (see docs/ROADMAP.md)
-- Source      : GHC users guide — FlexibleInstances
--
-- == Concept
-- Lifts Haskell 98's rule that an instance head must be of the form
-- @C (T a1 a2 ...)@ with each @ai@ a fresh variable. With this
-- extension, instance heads may mention concrete types or nested
-- constructors, e.g. @instance C (Maybe Int)@.
module Exts.TypeClasses.FlexibleInstances where

{-
The FlexibleInstances extension allows the head of the instance declaration to
mention arbitrary nested types.
-}

class C a where

instance C (Maybe Int) where
