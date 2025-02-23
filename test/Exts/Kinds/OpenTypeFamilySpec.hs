{-# LANGUAGE TypeApplications #-}

module Exts.Kinds.OpenTypeFamilySpec (spec) where

import Test.Hspec
import Exts.Kinds.OpenTypeFamily

spec :: Spec
spec = do
  describe "showTypeName" $ do
    it "works with a type" $ do
      showTypeName @Int `shouldBe` "Int"

    it "works with a function" $ do
      showTypeName @(Int -> String) `shouldBe` "Int -> [Char]"

    it "works with a multi param function (currying)" $ do
      showTypeName @(Int -> Int -> String) `shouldBe` "Int -> Int -> [Char]"

    it "works with tuples" $ do
      showTypeName @(Char -> String, String -> Int) `shouldBe` "(Char -> [Char],[Char] -> Int)"

    it "works with list types" $ do
      showTypeName @[Int] `shouldBe` "[Int]"
