{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module ParsingFunction2 where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.Text (Text)
import Encoding

data Packable = forall a. Encode a => Packable { getPackable :: FileData a }

instance Encode Packable where
  encode (Packable p) = encode p

newtype FilePack = FilePack [Packable]

instance Encode FilePack where
  encode (FilePack p) = encode p

naiveDecodeWord32 :: ByteString -> Either String (Word32, ByteString)
naiveDecodeWord32 inputString = do
  when (BS.length inputString < 4) $
    Left "Error, not enough data to get the size of the next field"
  let (encodedSizePrefix, rest) = BS.splitAt 4 inputString
  sizePrefix <- fromIntegral <$> bytestringToWord32 encodedSizePrefix
  when (sizePrefix /=4) $
    Left "the field size of a word should be 4"
  when (BS.length rest < sizePrefix) $
    Left "Not enought data for the next field size"
  let (encodedWord, rest') = BS.splitAt sizePrefix rest
  decodedWord <- decode encodedWord
  pure (decodedWord, rest')

naiveDecodedString :: ByteString -> Either String (String, ByteString)
naiveDecodedString inputString = do
  when (BS.length inputString < 4) $
    Left "Error, not enough data to get the size of the next field"
  let (encodedSizePrefix, rest) = BS.splitAt 4 inputString
  sizePrefix <- fromIntegral <$> bytestringToWord32 encodedSizePrefix
  when (BS.length rest < sizePrefix) $
    Left "Not enough data for the next field size"
  let (encodedString, rest') = BS.splitAt sizePrefix rest
  decodedString <- decode encodedString
  pure (decodedString, rest')

extractBytes :: Int -> ByteString -> Either String (ByteString, ByteString)
extractBytes n byteString = do
  when (BS.length byteString < n) $
    Left $ "Error, extract bytes needs at least " <> show n <> " bytes"
  pure $ BS.splitAt n byteString

nextSegmentSize :: ByteString -> Either String (Word32, ByteString)
nextSegmentSize byteString = do
  (nextSegmentStr, rest) <- extractBytes 4 byteString
  parsedSegmentSize <- bytestringToWord32 nextSegmentStr
  pure (parsedSegmentSize, rest)

nextSegment :: ByteString -> Either String (ByteString, ByteString)
nextSegment byteString = do
  (segmentSize, rest) <- nextSegmentSize byteString
  extractBytes (fromIntegral segmentSize) rest

-- Test data

addFileDataToPack :: Encode a => FileData a -> FilePack -> FilePack
addFileDataToPack a (FilePack as) = FilePack $ (Packable a) : as

infixr 6 .:
(.:) :: (Encode a) => FileData a -> FilePack -> FilePack
(.:) = addFileDataToPack

emptyFilePack :: FilePack
emptyFilePack = FilePack []

testEncodeValue :: ByteString
testEncodeValue =
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
  in encode $ a .: b .: c .: emptyFilePack
