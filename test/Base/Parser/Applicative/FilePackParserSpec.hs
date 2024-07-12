{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Base.Parser.Applicative.FilePackParserSpec where

import Test.Hspec
import Base.Parser.FilePackParser.Encoding
import Base.Parser.Applicative.FilePackParser
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word32)
import System.Directory (makeAbsolute)
import System.FilePath (dropFileName, (</>))

addFileDataToPack :: Encode a => FileData a -> FilePack -> FilePack
addFileDataToPack a (FilePack as) = FilePack $ Packable a : as

infixr 6 .:
(.:) :: (Encode a) => FileData a -> FilePack -> FilePack
(.:) = addFileDataToPack

emptyFilePack :: FilePack
emptyFilePack = FilePack []

testDecodeValue
  :: ByteString
  -> Either String
  ( FileData String
  , FileData [Text]
  , FileData (Word32,String)
  )
testDecodeValue = execParser $ (,,)
  <$> extractValue
  <*> extractValue
  <*> extractValue

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
        encode (Packable fileDataSample) `shouldBe` expectedPayload
        encode fileDataSample `shouldBe` expectedPayload

      it "decode value" $ do
        testDataFile <- (</> "FileData_String.bin") <$> currentTestDataDir
        payload <- BS.readFile testDataFile
        decode @(FileData String) payload `shouldBe` (Right fileDataSample)

      it "runs round trip" $ do
        decode (encode @(FileData String) fileDataSample) `shouldBe` Right fileDataSample

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
        decode @(FileData (Word32,String)) payload `shouldBe` (Right fileDataSample)

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

    -- it "encode values" $ do
    --   testDataFile <- (</> "FilePack.bin") <$> currentTestDataDir
    --   -- Uncomment line to regenerate content
    --   -- BS.writeFile testDataFile (encode $ )
    --   expectedPayload <- BS.readFile testDataFile
    --   payload `shouldBe` expectedPayload

    it "decode values" $ do
      testDataFile <- (</> "FilePack.bin") <$> currentTestDataDir
      payload <- BS.readFile testDataFile

      -- putStrLn $ "payload: " <> (BS.unpack payload)

      "" `shouldBe` ""

-- testRoundTrip :: (Encode a, Decode a, Show a, Eq a) => a -> IO ()
-- testRoundTrip val =
--   case decode (encode val) of
--     Left err ->
--       putStrLn $ "Fail to round-trip value: " <> err
--     Right roundTripVal
--       | roundTripVal == val ->
--           putStrLn "it works!"
--       | otherwise -> do
--         putStrLn "Round-trip failed!"
--         putStrLn $ "expected: " <> show val
--         putStrLn $ "got: " <> show roundTripVal

-- runRoundTripTest :: IO ()
-- runRoundTripTest =
--   testRoundTrip $ FileData
--   { fileName = "c"
--   , fileSize = 8
--   , filePermissions = 0644
--   , fileData = (0, "zero") :: (Word32,String)
--   }

-- Test data
-- cabal repl
-- testDecodeValue FilePackParser3.testEncodeValue
