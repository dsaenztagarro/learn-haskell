{-# LANGUAGE OverloadedStrings #-}
module Apps.Mtl.StateDemo1Spec where

import Test.Hspec
import Apps.Mtl.StateDemo1
import Std.Control.Monad.State
import Data.Text as Text

spec :: Spec
spec = do
  describe "parseFullName" $ do
    it "parses correctly" $ do
      let initialState = "Mike Robinson Crusoe"
      (evalState parseFullName initialState) `shouldBe` (Right $ FullName "Mike" "Robinson" "Crusoe")
