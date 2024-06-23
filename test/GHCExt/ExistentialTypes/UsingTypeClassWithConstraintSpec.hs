module GHCExt.ExistentialTypes.UsingTypeClassWithConstraintSpec where

import Test.Hspec
import GHCExt.ExistentialTypes.UsingTypeClassWithConstraint

spec :: Spec
spec = do
  describe "CanBeShown" $ do
    it "runs with type class instance for Int" $ do
      let collection = [CanBeShown "hello", CanBeShown (12 :: Int), CanBeShown True]
      map show collection `shouldBe` ["\"hello\"", "12", "True"]
