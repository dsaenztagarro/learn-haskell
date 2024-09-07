{-# LANGUAGE TypeApplications #-}

module GHCExt.InferredTypesSpec (spec) where

import Test.Hspec
import GHCExt.InferredTypes

spec :: Spec
spec = do

  {-
  Example where all type variables are specified types

  :t fromIntegral

  fromIntegral :: (Integral a, Num b) => a -> b

  :set -fprint-explicit-foralls
  :t fromIntegral

  fromIntegral :: forall a b. (Integral a, Num b) => a -> b
  -}
  describe "fromIntegral" $ do
    it "allows type application in all specified types" $ do
      fromIntegral @Int @Double (3 :: Int) `shouldBe` 3.0

    it "allows type application on output while keeping input polymorphic" $ do
      fromIntegral @_ @Float (3 :: Word) `shouldBe` 3.0

  -- Examples with both inferred and specified types

  describe "convertViaInt" $ do
    it "allows type application on output specified type, skipping inferred input type" $ do
      convertViaInt @Double (3 :: Int) `shouldBe` 3.0

  describe "convertViaInt'" $ do
    it "allows type application on output specified type, skipping inferred input type" $ do
      convertViaInt' @Double (3 :: Int) `shouldBe` 3.0
