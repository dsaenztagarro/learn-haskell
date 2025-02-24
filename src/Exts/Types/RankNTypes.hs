-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

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
