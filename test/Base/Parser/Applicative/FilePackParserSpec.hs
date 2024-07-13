{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Base.Parser.Applicative.FilePackParserSpec where

import Test.Hspec
import Base.Parser.FilePackParser.Encoding
import Base.Parser.Applicative.FilePackParser
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word32)
import System.Directory (makeAbsolute)
import System.FilePath (dropFileName, (</>))

currentTestDataDir :: IO String
currentTestDataDir =
  makeAbsolute (dropFileName __FILE__) >>=
    \dir -> return (dir </> ".." </> "testdata")

spec :: Spec
spec = do
  -- Parsing a Single Value
  describe "Packable" $ do
    describe "FileData String" $ do
      let
        fileDataSample = FileData
          { fileName = "a"
          , fileSize = 3
          , filePermissions = 0755
          , fileData = "foo" :: String
          }

      it "encode value" $ do
        testDataFile <- (</> "FileData_String.bin") <$> currentTestDataDir
        -- Uncomment to regenerate testdata
        -- BS.writeFile testDataFile (encode $ Packable fileDataSample)
        expectedPayload <- BS.readFile testDataFile
        encode fileDataSample `shouldBe` expectedPayload
        encode (Packable fileDataSample) `shouldBe` expectedPayload

      it "decode value" $ do
        testDataFile <- (</> "FileData_String.bin") <$> currentTestDataDir
        payload <- BS.readFile testDataFile
        decode @(FileData String) payload `shouldBe` Right fileDataSample
        {-
        `decode @Packable payload` won't work, because `Packable` type
        constructor is using flag `ExistentialQuantification` to hide the `a`
        type for `FileData a` in the data constructor.
        -}

      it "runs round trip" $ do
        decode (encode @(FileData String) fileDataSample) `shouldBe` Right fileDataSample
        decode (encode fileDataSample) `shouldBe` Right fileDataSample

    describe "FileData (Word32,String)" $ do
      let
        fileDataSample = FileData
          { fileName = "c"
          , fileSize = 8
          , filePermissions = 0644
          , fileData = (0, "zero") :: (Word32,String)
          }

      it "encode value" $ do
        testDataFile <- (</> "FileData_Word32_String.bin") <$> currentTestDataDir
        -- Uncomment to regenerate testdata
        -- BS.writeFile testDataFile (encode $ Packable fileDataSample)
        expectedPayload <- BS.readFile testDataFile
        encode (Packable fileDataSample) `shouldBe` expectedPayload
        encode fileDataSample `shouldBe` expectedPayload

      it "decode value" $ do
        testDataFile <- (</> "FileData_Word32_String.bin") <$> currentTestDataDir
        payload <- BS.readFile testDataFile
        decode @(FileData (Word32,String)) payload `shouldBe` Right fileDataSample
        {-
        `decode @Packable payload` won't work, because `Packable` type
        constructor is using flag `ExistentialQuantification` to hide the `a`
        type for `FileData a` in the data constructor.
        -}

      it "runs round trip" $ do
        decode (encode @(FileData (Word32,String)) fileDataSample) `shouldBe` Right fileDataSample

  -- Parsing a List of Values
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
      filePackSample = encode $ a .: b .: c .: emptyFilePack

    it "encode values" $ do
      testDataFile <- (</> "FilePack.bin") <$> currentTestDataDir
      -- Uncomment line to regenerate content
      -- BS.writeFile testDataFile (encode filePackSample)
      expectedPayload <- BS.readFile testDataFile
      encode filePackSample `shouldBe` expectedPayload

    describe "decodeThree" $ do
      it "decode values" $ do
        testDataFile <- (</> "FilePack.bin") <$> currentTestDataDir
        payload <- BS.readFile testDataFile
        decodeThree payload `shouldBe` Right (a,b,c)

    describe "parseThree" $ do
      let
        parseThree = (,,)
          <$> (extractValue @(FileData String))
          <*> (extractValue @(FileData [Text]))
          <*> (extractValue @(FileData (Word32,String)))

      it "decode values" $ do
        testDataFile <- (</> "FilePack.bin") <$> currentTestDataDir
        payload <- BS.readFile testDataFile
        execParser parseThree payload `shouldBe` Right (a,b,c)

    describe "parseMany" $ do
      describe "same type" $ do
        let
          d = FileData
            { fileName = "a"
            , fileSize = 3
            , filePermissions = 0755
            , fileData = "foo" :: String
            }
          e = FileData
            { fileName = "b"
            , fileSize = 10
            , filePermissions = 0644
            , fileData = "hello" :: String
            }
          filePackSample = encode $ a .: d .: e .: emptyFilePack

        it "decode values" $ do
          execParser (parseMany (extractValue @(FileData String))) filePackSample `shouldBe` Right [a,d,e]

    describe "parseSome" $ do
      let
        d = FileData
          { fileName = "a"
          , fileSize = 3
          , filePermissions = 0755
          , fileData = "foo" :: String
          }
        e = FileData
          { fileName = "c"
          , fileSize = 8
          , filePermissions = 0644
          , fileData = (0,"zero") :: (Word32,String)
          }
        filePackSample = encode $ a .: d .: e .: emptyFilePack

      it "decode values" $ do
        let decodedValue = execParser (parseSome (extractValue @(FileData String))) filePackSample

        length <$> decodedValue `shouldBe` Right 3
        take 2 <$> decodedValue `shouldBe` Right [a,d]
        take 2 <$> decodedValue `shouldBe` Right [a,d]
