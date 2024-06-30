{-# LANGUAGE TypeApplications #-}

module Base.HigherKindedTypeSpec (spec) where

import Test.Hspec
import Base.HigherKindedType

spec :: Spec
spec = do
  describe "toCSV" $ do
    it "returns csv for Maybe" $ do
      toCSV @_ @Int Nothing `shouldBe` ""
      toCSV @Maybe @Int Nothing `shouldBe` ""
      
      (toCSV @_ @Float $ Just 1) `shouldBe` "1.0"
      (toCSV @_ @Int $ Just 1) `shouldBe` "1"

      toCSV [1,2,3] `shouldBe` "1,2,3"

      (toCSV @(Either Int) @Int $ Right 3) `shouldBe` "3"
