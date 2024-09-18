{-# LANGUAGE DataKinds #-}
module YouTube.Kinds2Spec (spec) where

import Test.Hspec
import Data.Kind
import YouTube.Kinds2
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "fmapC" $ do
    it "behaves like fmap" $ do
      s3 `shouldBe` Set.fromList [3,4,5]

  describe "IntMod" $ do
    it "sums depending on module" $ do
      (4 + 3 :: IntMod 5) `shouldBe` MkIM 2
      (4 + 3 :: IntMod 6) `shouldBe` MkIM 1
      (4 + 3 :: IntMod 4) `shouldBe` MkIM 3
