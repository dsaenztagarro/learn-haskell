{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Apps.FilePack.Encoder1
-- Stage       : 08-Apps  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Building applications chapter (FilePack)
--
-- == Concept
-- First iteration of the FilePack serializer: pack a list of
-- 'FileData' into a base64-encoded 'ByteString' using @Show@ +
-- @readEither@. Naive but enough to round-trip. The next two
-- iterations ('Encoder2', 'Encoder3') tighten this up.
module Apps.FilePack.Encoder1 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import System.Posix.Types (FileMode, CMode(..))
import Text.Read (readEither)

data FileContents
  = StringFileContents String
  | TextFileContents Text
  | ByteStringFileContents ByteString
  deriving (Eq, Read, Show)

data FileData = FileData
  { fileName :: FilePath
  , fileSize :: Word32
  , filePermissions :: FileMode
  , fileData :: FileContents
  } deriving (Eq, Read, Show)

newtype FilePack =
  FilePack { getPackedFiles :: [FileData] } deriving (Eq, Read, Show)

packFiles :: FilePack -> ByteString
packFiles filePack =
  B64.encode . BC.pack . show $ filePack

unpackFiles :: ByteString -> Either String FilePack
unpackFiles serializedData =
  B64.decode serializedData >>= readEither . BC.unpack
