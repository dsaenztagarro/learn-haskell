module Exts.Deriving.AnyclassDerivingSpec (spec) where

import Test.Hspec
import Exts.Deriving.AnyclassDeriving

spec :: Spec
spec = do
  it "derives type class instance" $ do
    (redacted $ UserName "george") `shouldBe` "UserName \"george\""
