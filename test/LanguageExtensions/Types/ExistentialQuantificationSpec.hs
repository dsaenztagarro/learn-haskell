module LanguageExtensions.Types.ExistentialQuantificationSpec (spec) where

import Test.Hspec
import LanguageExtensions.Types.ExistentialQuantification

spec :: Spec
spec = do
  describe "render" $ do
    it "renders for different existential types" $ do
      render (inc counterA) `shouldBe` "1"
      render (inc (inc counterB)) `shouldBe` "\"##\""

