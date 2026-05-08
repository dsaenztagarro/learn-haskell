{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Exts.UndecidableInstances
-- Stage       : 06-Kinds  (see docs/ROADMAP.md)
-- Source      : GHC users guide — UndecidableInstances
--               https://en.wikipedia.org/wiki/Undecidable_problem
-- Prereqs     : Exts.TypeClasses.FlexibleInstances
--
-- == Concept
-- GHC's instance-resolution termination check (the /Paterson conditions/)
-- requires every constraint in the context to be /strictly smaller/ than
-- the instance head. The instance below violates that — @D a@ in the
-- context has the same complexity as @C a@ in the head — so without
-- @UndecidableInstances@ GHC refuses it on the grounds that it could
-- send the type checker into an infinite loop. The extension says
-- "I have argued informally that resolution terminates; trust me."
--
-- == Example
-- >>> c (5 :: Int)
-- "D Int: 5"
--
-- == Exercise
-- Try removing the @UndecidableInstances@ pragma. The resulting GHC
-- error message is the canonical "constraint not smaller than head"
-- message; recognising it on sight is the lesson.
module Exts.UndecidableInstances where

class C a where
  c :: a -> String

class D a where
  d :: a -> String

-- The constraint @D a@ has the same size as the head @C a@, so the
-- Paterson termination check fails without @UndecidableInstances@.
instance D a => C a where
  c = d

instance D Int where
  d n = "D Int: " ++ show n
