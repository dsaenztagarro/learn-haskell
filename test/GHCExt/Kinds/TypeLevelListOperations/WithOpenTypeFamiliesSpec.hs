{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module GHCExt.Kinds.TypeLevelListOperations.WithOpenTypeFamiliesSpec where

import Test.Hspec
import GHCExt.Kinds.TypeLevelListOperations.WithOpenTypeFamilies
import Data.Proxy

-- -----------------------------------------------------------
-- IMPORTANT! Type level specs will fail at compilation time.
-- -----------------------------------------------------------

data Foo
data Bar

spec :: Spec
spec = do
  describe "Open type families" $ do
    describe "IfThenElse" $ do
      it "returns True when is a member" $ do
        Proxy @(IfThenElse True Foo Bar) `shouldBe` Proxy @Foo

      it "returns False when is not a member" $ do
        Proxy @(IfThenElse False Foo Bar) `shouldBe` Proxy @Bar

    describe "EQ" $ do
      it "returns True when applied same type" $ do
        Proxy @(EQ Foo Foo) `shouldBe` Proxy @True
      it "returns False when applied different types" $ do
        Proxy @(EQ Foo Bar) `shouldBe` Proxy @False

    describe "FindElems" $ do
      describe "LessThanOrEqual" $ do
        it "finds expected elements" $ do
          Proxy @(FindElems (LessThanOrEqual 3) '[1,2,3,4,5,6]) `shouldBe` Proxy @('[1,2,3])

      describe "Even" $ do
        it "finds expected elements" $ do
          Proxy @(FindElems Even '[1,2,3,4,5,6]) `shouldBe` Proxy @('[2,4,6])
