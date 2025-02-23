module LanguageExtensions.Types.ExistentialQuantification.WithRecordSpec where

import Test.Hspec
import LanguageExtensions.Types.ExistentialQuantification.WithRecord

spec :: Spec
spec = do
  describe "runExistential" $ do
    it "runs with addAndMultiplyInt" $ do
      runExistential (addAndMultiplyInt (3 :: Int)) `shouldBe` 18

    it "runs with reverseAndUnwordsString" $ do
      runExistential (reverseAndUnwordsString "Hello, world") `shouldBe` "dlrow ,olleH Hello, world"

    it "runs with reverseAndUnwordsString" $ do
      runExistential (constExistential 7) `shouldBe` 7
