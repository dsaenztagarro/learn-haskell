{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Apps.FilePack.Util where

import Data.Bits ((.&.), (.|.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word
import System.Posix.Types (FileMode, CMode(..))
import Text.Printf

data FileData a = FileData
  { fileName :: FilePath
  , fileSize :: Word32
  , filePermissions :: FileMode
  , fileData :: a
  } deriving (Eq, Read, Show)

----------------
-- Type classes
----------------

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

-------------------------------
-- Encode type class instances
-------------------------------

instance Encode ByteString where
  encode = id

instance Encode Text where
  encode = encodeUtf8

-- Without language extension FlexibleInstances we are not allowed to create an
-- instance of a type class that specifies a particular value for a type parameter.
-- Since String is an alias for [Char], it is not allowed to create an instance
-- for it. Therefore it would be needed to create a more general instance for [a].
--
-- To avoid the conflict with the instance for Encode [a], we could have used
-- the OVERLAPS pragma, to stay which instance should be authorative when conflict.
--
-- instance {-# OVERLAPS #-} Encode String where
--
instance Encode String where
  encode = BC.pack

instance Encode Word32 where
  encode = word32ToByteString
  encodeWithSize w =
    let (a, b, c, d) = word32ToBytes w
    in BS.pack [ 4, 0, 0, 0
               , a, b, c, d]

instance Encode Word16 where
  encode = word16ToByteString

-- FileMode is an alias for CMode, which is a newtype wrapper around Word32
instance Encode FileMode where
  encode (CMode fMode) = encode fMode

instance Encode a => Encode (FileData a) where
  encode FileData{..} = encode $
    encodeWithSize fileName
    <> encodeWithSize fileSize
    <> encodeWithSize filePermissions
    <> encodeWithSize fileData

instance (Encode a, Encode b) => Encode (a,b) where
  encode (a,b) =
    encode $ encodeWithSize a <> encodeWithSize b

{-
The following instance overlaps with the one defined for String.
By using the OVERLAPPABLE pragma we can tell GHC to always prefer a different
instance if there happens to be a conflict.
In other words, use OVERLAPPABLE instance to specify an instance that should NOT
be chosen if there is a conflict.
-}
instance {-# OVERLAPPABLE #-} Encode a => Encode [a] where
  encode = encode . foldMap encodeWithSize

-------------------------------
-- Decode type class instances
-------------------------------

instance Decode ByteString where
  decode = Right

instance Decode Text where
  decode = Right . decodeUtf8

instance Decode String where
  decode = Right . BC.unpack

instance Decode Word32 where
  decode = bytestringToWord32

instance Decode Word16 where
  decode = bytestringToWord16

instance Decode FileMode where
  decode = fmap CMode . decode

---------------------------
-- Word32 helper functions
---------------------------

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

consWord32 :: Word32 -> ByteString -> ByteString
consWord32 word bytestring =
  let packedWord = word32ToByteString word
  in packedWord <> bytestring

---------------------------
-- Word16 helper functions
---------------------------

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

---------------------------
-- Bits manipulation
---------------------------

-- showBinary = printf "%b\n"

printBytes :: (Word8, Word8, Word8, Word8) -> String
printBytes (a, b, c, d) = printf "%02x %02x %02x %02x\n" a b c d

-- Little Endian (least significant bytes first)
--
-- ghci> printBytes $ word32ToBytes 255
-- ff 00 00 00
-- ghci> word32ToBytes 0x000000ff
-- (255,0,0,0)
-- ghci> word32ToBytes $ shift 0x000000ff 8
-- (0,255,0,0)
-- ghci> word32ToBytes $ shift 0x000000ff 16
-- (0,0,255,0)
-- ghci> word32ToBytes $ shift 0x000000ff 24
-- (0,0,0,255)
