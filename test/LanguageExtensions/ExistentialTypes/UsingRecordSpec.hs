module LanguageExtensions.ExistentialTypes.UsingRecordSpec where

import Test.Hspec
import LanguageExtensions.ExistentialTypes.UsingRecord

spec :: Spec
spec = do
  describe "runExistential" $ do
    it "runs with addAndMultiplyInt" $ do
      runExistential (addAndMultiplyInt (3 :: Int)) `shouldBe` 18

    it "runs with reverseAndUnwordsString" $ do
      runExistential (reverseAndUnwordsString "Hello, world") `shouldBe` "dlrow ,olleH Hello, world"
