module YouTube.Kinds2Spec (spec) where

import Test.Hspec
import YouTube.Kinds2
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "fmapC" $ do
    it "behaves like fmap" $ do
      s3 `shouldBe` Set.fromList [3,4,5]
