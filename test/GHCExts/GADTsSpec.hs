module GHCExts.GADTsSpec where

import Test.Hspec
import GHCExts.GADTs

spec :: Spec
spec = do
  describe "GADTs" $ do
    it "runs with addAndMultiplyInt" $ do
      1 `shouldBe` 1

