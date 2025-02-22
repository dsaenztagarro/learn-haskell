module LanguageExtensions.Deriving.AnyclassDerivingSpec (spec) where

import Test.Hspec
import LanguageExtensions.Deriving.AnyclassDeriving

spec :: Spec
spec = do
  it "derives type class instance" $ do
    (redacted $ UserName "george") `shouldBe` "UserName \"george\""
