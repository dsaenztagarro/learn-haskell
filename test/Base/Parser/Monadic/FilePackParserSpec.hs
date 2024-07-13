{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Base.Parser.Monadic.FilePackParserSpec where

import Test.Hspec
import Base.Parser.FilePackParser.Encoding
import Base.Parser.Applicative.FilePackParser
import Base.Parser.Monadic.FilePackParser
import qualified Data.ByteString as BS
import System.Directory (makeAbsolute)
import System.FilePath (dropFileName, (</>))

currentTestDataDir :: IO String
currentTestDataDir =
  makeAbsolute (dropFileName __FILE__) >>=
    \dir -> return (dir </> ".." </> "testdata")

addFileDataToPack :: Encode a => FileData a -> FilePack -> FilePack
addFileDataToPack a (FilePack as) = FilePack $ Packable a : as

spec :: Spec
spec = do
  describe "FilePackPBM" $ do
    let pbm = FilePackPBM 100 200 [25,50,75]

    it "encode value" $ do
      testDataFile <- (</> "FilePackPBM.bin") <$> currentTestDataDir
      -- Uncomment to regenerate testdata
      BS.writeFile testDataFile (encode pbm)
      expectedPayload <- BS.readFile testDataFile
      encode pbm `shouldBe` expectedPayload

    it "decode value" $ do
      testDataFile <- (</> "FilePackPBM.bin") <$> currentTestDataDir
      payload <- BS.readFile testDataFile
      decode payload `shouldBe` Right pbm

    it "runs round trip" $ do
      decode (encode pbm) `shouldBe` Right pbm

  describe "FilePackPGM" $ do
    let pgm = FilePackPGM 100 200 300 [25,50,75]

    it "encode value" $ do
      testDataFile <- (</> "FilePackpgm.bin") <$> currentTestDataDir
      -- Uncomment to regenerate testdata
      BS.writeFile testDataFile (encode pgm)
      expectedPayload <- BS.readFile testDataFile
      encode pgm `shouldBe` expectedPayload

    it "decode value" $ do
      testDataFile <- (</> "FilePackpgm.bin") <$> currentTestDataDir
      payload <- BS.readFile testDataFile
      decode payload `shouldBe` Right pgm

    it "runs round trip" $ do
      decode (encode pgm) `shouldBe` Right pgm

  describe "[FilePackImage]" $ do
    let
      pbm = FilePackPBM 100 200 [25,50,75]
      pgm = FilePackPGM 100 200 300 [25,50,75]

    it "runs round trip" $ do
      decode (encode [pgm, pbm]) `shouldBe` Right [pgm, pbm]

  describe "FilePack" $ do
    let
      pbm = FilePackPBM 100 200 [25,50,75]
      pgm = FilePackPGM 100 200 300 [25,50,75]
      payload = encode [pbm, pgm, pgm, pbm]

    it "decode values" $ do
      execParser (parseMany (extractValue @FilePackImage)) payload `shouldBe` Right [pbm, pgm, pgm, pbm]
