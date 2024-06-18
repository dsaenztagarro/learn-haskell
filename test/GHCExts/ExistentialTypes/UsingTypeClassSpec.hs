module GHCExts.ExistentialTypes.UsingTypeClassSpec where

import Test.Hspec
import GHCExts.ExistentialTypes.UsingTypeClass

spec :: Spec
spec = do
  describe "runSomeClass" $ do
    it "runs with type class instance for Int" $ do
      (runSomeClass 3 :: Int) `shouldBe` 18
