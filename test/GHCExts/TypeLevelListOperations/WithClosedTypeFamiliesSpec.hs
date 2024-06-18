{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module GHCExts.TypeLevelListOperations.WithClosedTypeFamiliesSpec where

import Test.Hspec
import GHCExts.TypeLevelListOperations.WithClosedTypeFamilies
import Data.Proxy

-- -----------------------------------------------------------
-- IMPORTANT! Type level specs will fail at compilation time.
-- -----------------------------------------------------------

data Foo
data Bar

spec :: Spec
spec = do
  describe "Closed type families" $ do
    describe "Member" $ do
      it "returns True when is a member" $ do
        Proxy @(Member 1 '[1,2,3]) `shouldBe` Proxy @True

      it "returns False when is not a member" $ do
        Proxy @(Member 5 '[1,2,3]) `shouldBe` Proxy @False

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

    describe "EvalEven" $ do
      it "returns True when applied same type" $ do
        Proxy @(EvalEven (Even 3)) `shouldBe` Proxy @False
      it "returns False when applied different types" $ do
        Proxy @(EvalEven (Even 4)) `shouldBe` Proxy @True

    describe "FindElems" $ do
      it "finds even elements" $ do
        Proxy @(FindElems Even '[1,2,3,4,5,6]) `shouldBe` Proxy @('[2,4,6])
