{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module GHCExt.GADT.CommandRunnerSpec (spec) where

import Test.Hspec
import GHCExt.GADT.CommandRunner
import GHCExt.GADT.ShellCmd

spec :: Spec
spec = do
  describe "CommandSet" $ do
    it "creates a set of commands" $ do
      let commandSet = AddCommand @"ls" listDirectory EmptyCommandSet
      1 `shouldBe` 1


