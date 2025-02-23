{-# LANGUAGE OverloadedStrings #-}
module Apps.FilePack.Encoder3Spec where

import Test.Hspec
import Apps.FilePack.Encoder3
import Apps.FilePack.Util
import Data.Text (Text)
import Data.Word

spec :: Spec
spec = do
  describe "FilePack" $ do
    let
      a = FileData
        { fileName = "a"
        , fileSize = 3
        , filePermissions = 0755
        , fileData = "foo" :: String
        }
      b = FileData
        { fileName = "b"
        , fileSize = 10
        , filePermissions = 0644
        , fileData = ["hello", "world"] :: [Text]
        }
      c = FileData
        { fileName = "c"
        , fileSize = 8
        , filePermissions = 0644
        , fileData = (0,"zero") :: (Word32,String)
        }

    it "encodes a filepack" $ do
      (encode $ a .: b .: c .: emptyFilePack) `shouldBe` "\SUB\NUL\NUL\NUL\SOH\NUL\NUL\NULa\EOT\NUL\NUL\NUL\ETX\NUL\NUL\NUL\STX\NUL\NUL\NULó\STX\ETX\NUL\NUL\NULfoo)\NUL\NUL\NUL\SOH\NUL\NUL\NULb\EOT\NUL\NUL\NUL\n\NUL\NUL\NUL\STX\NUL\NUL\NUL\132\STX\DC2\NUL\NUL\NUL\ENQ\NUL\NUL\NULhello\ENQ\NUL\NUL\NULworld'\NUL\NUL\NUL\SOH\NUL\NUL\NULc\EOT\NUL\NUL\NUL\b\NUL\NUL\NUL\STX\NUL\NUL\NUL\132\STX\DLE\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NULzero"
