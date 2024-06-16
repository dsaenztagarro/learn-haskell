{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHCExts.ClosedTypeFamilySpec where

import Test.Hspec
import GHCExts.ClosedTypeFamily

spec :: Spec
spec = do
  describe "ToPeano closed type familiy" $ do
    it "converts 3 to Succ (Succ (Succ Zero))" $ do
      let testToPeano3 :: AssertEqual (ToPeano 3) (Succ (Succ (Succ Zero)))
          testToPeano3 = assertEqual
      testToPeano3 `seq` True `shouldBe` True
