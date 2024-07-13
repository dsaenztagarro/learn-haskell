{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Base.Parser.Applicative.FilePackParser where

import Control.Applicative
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Base.Parser.FilePackParser.Encoding
import Data.Text (Text)
import Data.Word (Word32)

data Packable = forall a. Encode a => Packable { getPackable :: FileData a }

instance Encode Packable where
  encode (Packable p) = encode p

newtype FilePack = FilePack [Packable]

instance Encode FilePack where
  encode (FilePack p) = encode p

{-
`FilePackParser` represent the type of _all_ functions defined to parse a
"fragment" of a bytestring.
When defined `Applicative` on `FilePackParser`, allows `a` to be a `FileData`
constructor, and so let us compose multiple parsers each of them using for
`decode` an instance of the related data constructor parameter.
-}
newtype FilePackParser a = FilePackParser
  { runParser :: ByteString -> Either String (a, ByteString) }

instance Functor FilePackParser where
  fmap f parser = FilePackParser $ \input -> do
    (parsedValue, result) <- runParser parser input
    pure (f parsedValue, result)

instance Applicative FilePackParser where
  pure a = FilePackParser $ \input -> pure (a, input)
  f <*> s = FilePackParser $ \input -> do
    (f', initialRemainder) <- runParser f input
    (a, finalRemainder) <- runParser s initialRemainder
    pure (f' a, finalRemainder)

instance Alternative FilePackParser where
  empty = FilePackParser $ const (Left "empty parser")
  parserA <|> parserB = FilePackParser $ \s ->
    case runParser parserA s of
      Right val -> Right val
      Left _errA -> runParser parserB s

extractValue :: Decode a => FilePackParser a
extractValue = FilePackParser $ \input -> do
  when (BS.length input < 4) $
    Left "Input has less than 4 bytes, we can't get a segment size"

  let (rawSegmentSize, rest) = BS.splitAt 4 input
  segmentSize <- fromIntegral <$> bytestringToWord32 rawSegmentSize

  when (BS.length rest < segmentSize) $
    Left "not enough input to parse the next value"

  let (rawSegmentValue, rest') = BS.splitAt segmentSize rest

  case decode rawSegmentValue of
    Left err -> Left err
    Right a -> Right (a, rest')

extractValues :: Decode a => FilePackParser [a]
extractValues = FilePackParser $ \input ->
  if BS.null input
  then Right ([], "")
  else do
    (val, rest) <- runParser extractValue input
    (vals, rest') <- runParser extractValues rest
    pure (val:vals, rest')

extractOptional :: FilePackParser a -> FilePackParser (Maybe a)
extractOptional = optional
{-
extractOptional parser = Just <$> parser <|> pure Nothing
extractOptional = (<|> pure Nothing) . fmap Just

extractOptional parser = FilePackParser $ \input ->
  case runParser parser input of
    Left _err -> pure (Nothing, input)
    Right (val, rest) -> pure (Just val, rest)
-}

addFileDataToPack :: Encode a => FileData a -> FilePack -> FilePack
addFileDataToPack a (FilePack as) = FilePack $ Packable a : as

infixr 6 .:
(.:) :: (Encode a) => FileData a -> FilePack -> FilePack
(.:) = addFileDataToPack

emptyFilePack :: FilePack
emptyFilePack = FilePack []

decodeThree
  :: ByteString
  -> Either String
  ( FileData String
  , FileData [Text]
  , FileData (Word32,String)
  )
decodeThree = execParser $ (,,)
  <$> extractValue
  <*> extractValue
  <*> extractValue

parseSome :: FilePackParser a -> FilePackParser [a]
parseSome p = (:) <$> p <*> parseMany p

parseMany :: FilePackParser a -> FilePackParser [a]
parseMany p = parseSome p <|> pure []
{-
parseMany parser = FilePackParser $ \input ->
  case runParser parser input of
    Left _err ->
      pure ([], input)
    Right (val, rest) -> do
      (tail, rest') <- runParser (parseMany parser) rest
      pure (val:tail, rest')
-}

execParser :: FilePackParser a -> ByteString -> Either String a
execParser parser inputString =
  fst <$> runParser parser inputString

instance (Decode a, Decode b) => Decode (a,b) where
  decode = execParser $ (,) <$> extractValue <*> extractValue

instance {-# OVERLAPPABLE #-} (Decode a) => Decode [a] where
  decode = execParser (many extractValue)

instance (Decode a) => Decode (FileData a) where
  decode = execParser $ FileData
    <$> extractValue <*> extractValue <*> extractValue <*> extractValue
