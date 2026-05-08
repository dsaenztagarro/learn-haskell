module Exts.UndecidableInstancesSpec (spec) where

import Test.Hspec
import Exts.UndecidableInstances

spec :: Spec
spec = do
  describe "C via D (UndecidableInstances)" $ do
    it "delegates c to d through the recursive instance" $
      c (5 :: Int) `shouldBe` "D Int: 5"
    it "delegates c on a different value" $
      c (0 :: Int) `shouldBe` "D Int: 0"
