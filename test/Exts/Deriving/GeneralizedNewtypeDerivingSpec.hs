module Exts.Deriving.GeneralizedNewtypeDerivingSpec (spec) where

import Test.Hspec
import Exts.Deriving.GeneralizedNewtypeDeriving

spec :: Spec
spec = do
  it "derives type class instance" $ do
    (USD 20) + (USD 30) `shouldBe` (USD 50) -- Num

    (USD 20 == USD 20) `shouldBe` True -- Eq
    (show $ USD 20) `shouldBe` "USD {getMillis = 20}" -- Show


