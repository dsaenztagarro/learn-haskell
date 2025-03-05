{-# LANGUAGE OverloadedStrings #-}
module Apps.Mtl.StateMonadWithTypeAliasSpec where

import Test.Hspec
import Apps.Mtl.StateMonadWithTypeAlias
import Libs.Mtl.Control.Monad.State
import Data.Text as Text

spec :: Spec
spec = do
  describe "parseFullName" $ do
    it "parses correctly" $ do
      let initialState = "Mike Robinson Crusoe"
      (evalState parseFullName initialState) `shouldBe` (Right $ FullName "Mike" "Robinson" "Crusoe")

    it "parses with error missing word" $ do
      let initialState = "Mike"
      (evalState parseFullName initialState) `shouldBe` (Left "missing word")
