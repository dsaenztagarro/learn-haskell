module Exts.Types.ExistentialQuantification.WithTypeClassSpec where

-- WARNING: This is NOT ExistentialQuantification
-- Just simulating existential quantification by only using Type classes

import Test.Hspec
import Exts.Types.ExistentialQuantification.WithTypeClass

spec :: Spec
spec = do
  describe "runSomeClass" $ do
    it "runs with type class instance for Int" $ do

      (runSomeClass 3 :: Int) `shouldBe` 18
