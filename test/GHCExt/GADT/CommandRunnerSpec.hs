{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module GHCExt.GADT.CommandRunnerSpec (spec) where

import Test.Hspec
import GHCExt.GADT.CommandRunner
import GHCExt.GADT.ShellCmd

verifyMkCommandSet :: 
  CommandSet 
    '["ls","free","uptime","uname"] 
    '[ ShellCmd FilePath [FilePath]
     , ShellCmd () String
     , ShellCmd () String
     , ShellCmd () String
     ] -> String
verifyMkCommandSet _ = "verified"

spec :: Spec
spec = do
  describe "mkCommandSet" $ do
    it "creates a set of commands" $ do
      verifyMkCommandSet mkCommandSet `shouldBe` "verified"
