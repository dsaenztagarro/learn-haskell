{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module ApplicativeParser where

import Control.Applicative
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word32)
import Encoding

-- parseFunction :: Decode a => ByteString -> Either String (a, ByteString)

data Packable = forall a. Encode a => Packable { getPackable :: FileData a }

instance Encode Packable where
  encode (Packable p) = encode p

newtype FilePack = FilePack [Packable]

instance Encode FilePack where
  encode (FilePack p) = encode p

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
      Left errA -> runParser parserB s

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
    (tail, rest') <- runParser extractValues rest
    pure (val:tail, rest')

extractOptional :: FilePackParser a -> FilePackParser (Maybe a)
extractOptional parser = Just <$> parser <|> pure Nothing
-- extractOptional = (<|> pure Nothing) . fmap Just
--
-- extractOptional parser = FilePackParser $ \input ->
--   case runParser parser input of
--     Left _err -> pure (Nothing, input)
--     Right (val, rest) -> pure (Just val, rest)

parseSome :: FilePackParser a -> FilePackParser [a]
parseSome p = (:) <$> p <*> parseMany p

parseMany :: FilePackParser a -> FilePackParser [a]
parseMany p = parseSome p <|> pure []

parseMany parser = FilePackParser $ \input ->
  case runParser parser input of
    Left _err ->
      pure ([], input)
    Right (val, rest) -> do
      (tail, rest') <- runParser (parseMany parser) rest
      pure (val:tail, rest')

execParser :: FilePackParser a -> ByteString -> Either String a
execParser parser inputString =
  fst <$> runParser parser inputString

instance (Decode a, Decode b) => Decode (a,b) where
  decode = execParser $ (,) <$> extractValue <*> extractValue

instance {-# OVERLAPPABLE #-} Decode a => Decode [a] where
  decode = execParser (many extractValue)

instance Decode a => Decode (FileData a) where
  decode = execParser $ FileData
    <$> extractValue <*> extractValue <*> extractValue <*> extractValue

testRoundTrip :: (Encode a, Decode a, Show a, Eq a) => a -> IO ()
testRoundTrip val =
  case decode (encode val) of
    Left err ->
      putStrLn $ "Fail to round-trip value: " <> err
    Right roundTripVal
      | roundTripVal == val ->
          putStrLn "it works!"
      | otherwise -> do
        putStrLn "Round-trip failed!"
        putStrLn $ "expected: " <> show val
        putStrLn $ "got: " <> show roundTripVal

runRoundTripTest :: IO ()
runRoundTripTest =
  testRoundTrip $ FileData
  { fileName = "c"
  , fileSize = 8
  , filePermissions = 0644
  , fileData = (0, "zero") :: (Word32,String)
  }

-- Test data
-- cabal repl
-- testDecodeValue FilePackParser3.testEncodeValue

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
