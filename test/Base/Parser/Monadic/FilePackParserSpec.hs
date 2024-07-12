module Base.Parser.Monadic.FilePackParserSpec where

import Test.Hspec
import Base.Parser.Monadic.FilePackParser

spec :: Spec
spec = do
  it "encode values" $ do
    1 `shouldBe` 1
