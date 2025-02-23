module LanguageExtensions.Types.ExistentialQuantification.WithTypeClassConstraintSpec where

import Test.Hspec
import LanguageExtensions.Types.ExistentialQuantification.WithTypeClassConstraint

spec :: Spec
spec = do
  describe "CanBeShown" $ do
    it "runs with type class instance for Int" $ do
      let collection = [CanBeShown "hello", CanBeShown (12 :: Int), CanBeShown True]
      map show collection `shouldBe` ["\"hello\"", "12", "True"]
