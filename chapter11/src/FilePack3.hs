{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module FilePack3 where

import Data.Bits ((.&.), (.|.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word
import System.Posix.Types (FileMode, CMode(..))
import Text.Printf
import Text.Read (readEither)

class Encode a where
  encode :: a -> ByteString
  encode = BS.drop 4 . encodeWithSize

  encodeWithSize :: a -> ByteString
  encodeWithSize a =
    let s = encode a
        l = fromIntegral $ BS.length s
    in word32ToByteString l <> s
  {-# MINIMAL encode | encodeWithSize #-}

class Decode a where
  decode :: ByteString -> Either String a

instance (Encode a, Encode b) => Encode (a,b) where
  encode (a,b) =
    encode $ encodeWithSize a <> encodeWithSize b

instance {-# OVERLAPPABLE #-} Encode a => Encode [a] where
  encode = encode . foldMap encodeWithSize

instance Encode ByteString where
  encode = id

instance Decode ByteString where
  decode = Right . id

instance Encode Text where
  encode = encodeUtf8

instance Decode Text where
  decode = Right . decodeUtf8

instance Encode String where
  encode = BC.pack

instance Decode String where
  decode = Right . BC.unpack

instance Encode Word32 where
  encode = word32ToByteString
  encodeWithSize w =
    let (a, b, c, d) = word32ToBytes w
    in BS.pack [ 4, 0, 0, 0
               , a, b, c, d]

instance Decode Word32 where
  decode = bytestringToWord32

instance Encode Word16 where
  encode = word16ToByteString

instance Decode Word16 where
  decode = bytestringToWord16

instance Encode FileMode where
  encode (CMode fMode) = encode fMode

instance Decode FileMode where
  decode = fmap CMode . decode

-- showBinary = printf "%b\n"
--
-- printBytes $ word32ToBytes 255
-- "ff 00 00 00\n"
-- Little endian x86-64 system -- least significant bytes first --

printBytes :: (Word8, Word8, Word8, Word8) -> String
printBytes (a, b, c, d) = printf "%02x %02x %02x %02x\n" a b c d


-- Word32 helper functions

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. shift word (-8)
      c = fromIntegral $ 255 .&. shift word (-16)
      d = fromIntegral $ 255 .&. shift word (-24)
  in (a,b,c,d)

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a,b,c,d) =
  let
    a' = fromIntegral a
    b' = shift (fromIntegral b) 8
    c' = shift (fromIntegral c) 16
    d' = shift (fromIntegral d) 24
  in a' .|. b' .|. c' .|. d'

word32ToByteString :: Word32 -> ByteString
word32ToByteString word =
  let (a,b,c,d) = word32ToBytes word
  in BS.pack [a,b,c,d]

bytestringToWord32 :: ByteString -> Either String Word32
bytestringToWord32 bytestring =
  case BS.unpack bytestring of
    [a,b,c,d] -> Right $ word32FromBytes (a,b,c,d)
    _otherwise ->
      let l = show $ BS.length bytestring
      in Left ("Expecting 4 bytes but got " <> l)

-- Word16 helper functions

word16ToBytes :: Word16 -> (Word8, Word8)
word16ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. shift word (-8)
  in (a,b)

word16FromBytes :: (Word8, Word8) -> Word16
word16FromBytes (a,b) =
  let
    a' = fromIntegral a
    b' = shift (fromIntegral b) 8
  in a' .|. b'

word16ToByteString :: Word16 -> ByteString
word16ToByteString word =
  let (a,b) = word16ToBytes word
  in BS.pack [a,b]

bytestringToWord16 :: ByteString -> Either String Word16
bytestringToWord16 bytestring =
  case BS.unpack bytestring of
    [a,b] -> Right $ word16FromBytes (a,b)
    _otherwise ->
      let l = show $ BS.length bytestring
      in Left ("Expecting 2 bytes but got " <> l)

consWord32 :: Word32 -> ByteString -> ByteString
consWord32 word bytestring =
  let packedWord = word32ToByteString word
  in packedWord <> bytestring

data FileData a = FileData
  { fileName :: FilePath
  , fileSize :: Word32
  , filePermissions :: FileMode
  , fileData :: a
  } deriving (Eq, Read, Show)

-- newtype FilePack a =
--   FilePack { getPackedFiles :: [FileData a] } deriving (Eq, Read, Show)

instance Encode a => Encode (FileData a) where
  encode FileData{..} =
    let
      encodedFileName = encodeWithSize fileName
      encodedFileSize = encodeWithSize fileSize
      encodedFilePermmissions = encodeWithSize filePermissions
      encodedFileData = encodeWithSize fileData
      encodedData =
        encodedFileName
        <> encodedFileSize
        <> encodedFilePermmissions
        <> encodedFileData
    in encode encodedData

-- instance Encode a => (FilePack a) where
--   encode (FilePack a) = FilePack2.encode a


data Packable = forall a. Encode a => Packable { getPackable :: FileData a }

instance Encode Packable where
  encode (Packable p) = encode p

newtype FilePack = FilePack [Packable]

instance Encode FilePack where
  encode (FilePack p) = encode p

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
  in encode $ a .: b .: emptyFilePack
