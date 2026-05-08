-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Exts.Types.RankNTypes
-- Stage       : 04-TypeSignatures  (see docs/ROADMAP.md)
-- Source      : GHC users guide — RankNTypes
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
