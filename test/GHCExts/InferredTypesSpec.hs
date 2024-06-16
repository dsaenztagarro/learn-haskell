{-# LANGUAGE TypeApplications #-}

module GHCExts.InferredTypesSpec (spec) where

import Test.Hspec
import GHCExts.InferredTypes

spec :: Spec
spec = do
  -- fromIntegral :: (Integral a, Num b) => a -> b
  describe "fromIntegral" $ do
    it "allows type application in all specified types" $ do
      fromIntegral @Int @Double (3 :: Int) `shouldBe` 3.0

    it "allows type application on output while keeping input polymorphic" $ do
      fromIntegral @_ @Float (3 :: Word) `shouldBe` 3.0

  describe "convertViaInt" $ do
    it "allows type application on output specified type, skipping inferred input type" $ do
      convertViaInt @Double (3 :: Int) `shouldBe` 3.0

  describe "convertViaInt'" $ do
    it "allows type application on output specified type, skipping inferred input type" $ do
      convertViaInt' @Double (3 :: Int) `shouldBe` 3.0
