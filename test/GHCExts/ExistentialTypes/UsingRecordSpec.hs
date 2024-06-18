module GHCExts.ExistentialTypes.UsingRecordSpec where

import Test.Hspec
import GHCExts.ExistentialTypes.UsingExistentialRecord

spec :: Spec
spec = do
  describe "runExistential" $ do
    it "runs with addAndMultiplyInt" $ do
      runExistential (addAndMultiplyInt 3) `shouldBe` 18

    it "runs with reverseAndUnwordsString" $ do
      runExistential (reverseAndUnwordsString "Hello, world") `shouldBe` "dlrow ,olleH Hello, world"
