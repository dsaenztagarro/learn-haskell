{-# LANGUAGE OverloadedStrings #-}
module Apps.FilePack.Serializer1Spec where

import Test.Hspec
import Apps.FilePack.Serializer1

spec :: Spec
spec = do
  describe "FilePack" $ do
    let
      sampleFilePack = FilePack $
        [ FileData "stringFile" 0 0 $ StringFileContents "hello string"
        , FileData "textFile" 0 0 $ TextFileContents "hello text"
        , FileData "binaryFile" 0 0 $ ByteStringFileContents "hello bytestring"
        ]
      serializedFilePack = "RmlsZVBhY2sge2dldFBhY2tlZEZpbGVzID0gW0ZpbGVEYXRhIHtmaWxlTmFtZSA9ICJzdHJpbmdGaWxlIiwgZmlsZVNpemUgPSAwLCBmaWxlUGVybWlzc2lvbnMgPSAwLCBmaWxlRGF0YSA9IFN0cmluZ0ZpbGVDb250ZW50cyAiaGVsbG8gc3RyaW5nIn0sRmlsZURhdGEge2ZpbGVOYW1lID0gInRleHRGaWxlIiwgZmlsZVNpemUgPSAwLCBmaWxlUGVybWlzc2lvbnMgPSAwLCBmaWxlRGF0YSA9IFRleHRGaWxlQ29udGVudHMgImhlbGxvIHRleHQifSxGaWxlRGF0YSB7ZmlsZU5hbWUgPSAiYmluYXJ5RmlsZSIsIGZpbGVTaXplID0gMCwgZmlsZVBlcm1pc3Npb25zID0gMCwgZmlsZURhdGEgPSBCeXRlU3RyaW5nRmlsZUNvbnRlbnRzICJoZWxsbyBieXRlc3RyaW5nIn1dfQ=="

    it "encode value" $ do
      packFiles sampleFilePack `shouldBe` serializedFilePack

    it "decode value" $ do
      unpackFiles serializedFilePack `shouldBe` Right sampleFilePack

    it "runs round trip" $ do
      (unpackFiles $ packFiles sampleFilePack) `shouldBe` Right sampleFilePack
