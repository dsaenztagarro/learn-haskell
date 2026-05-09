module Exts.Deriving.DeriveAnyClassSpec (spec) where

import Test.Hspec
import Exts.Deriving.DeriveAnyClass

spec :: Spec
spec = do
  it "derives type class instance" $ do
    (redacted $ UserName "george") `shouldBe` "UserName \"george\""
