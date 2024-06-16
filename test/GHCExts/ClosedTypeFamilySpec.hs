{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHCExts.ClosedTypeFamilySpec where

import Test.Hspec
import GHCExts.ClosedTypeFamily
import GHC.TypeLits (natVal)
import Data.Proxy

spec :: Spec
spec = do
  describe "Closed type families" $ do
    describe "ToPeano" $ do
      it "converts 3 to Succ (Succ (Succ Zero))" $ do
        let testToPeano3 :: AssertEqual (ToPeano 3) (Succ (Succ (Succ Zero)))
            testToPeano3 = assertEqual
        testToPeano3 `seq` True `shouldBe` True

    describe "ShowPeano" $ do
      it "shows Peano" $ do
        showPeanoName @(ToPeano 3) `shouldBe` "(Succ (Succ (Succ Zero)))"

    describe "FromPeano" $ do
      it "converts Peano to Natural" $ do
        3 `shouldBe` (natVal $ Proxy @(FromPeano (Succ (Succ (Succ Zero)))))

    describe "Add" $ do
      it "sums 2 Peano numbers" $ do
        8 `shouldBe` (natVal $ Proxy @(FromPeano (Add (ToPeano 3) (ToPeano 5))))

    describe "Multiply" $ do
      it "multiplies 2 Peano numbers" $ do
        15 `shouldBe` (natVal $ Proxy @(FromPeano (Multiply (ToPeano 3) (ToPeano 5))))

    describe "Substract" $ do
      it "subtracts 2 Peano numbers" $ do
        4 `shouldBe` (natVal $ Proxy @(FromPeano (Substract (ToPeano 7) (ToPeano 3))))
