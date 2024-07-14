{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}

module Base.Parser.Monadic.FilePackParser where

import Base.Parser.Applicative.FilePackParser
import Control.Applicative
import Data.Word (Word32)
import Base.Parser.FilePackParser.Encoding

instance Monad FilePackParser where
  return = pure
  valParser >>= mkParser = FilePackParser $ \input -> do
    (val, rest) <- runParser valParser input
    runParser (mkParser val) rest

instance MonadFail FilePackParser where
  fail errMsg = FilePackParser (const $ Left errMsg)

data FilePackImage
  = FilePackPBM Word32 Word32 [Word32]
  | FilePackPGM Word32 Word32 Word32 [Word32]
  | FilePackFOO Word32
  deriving (Eq, Show)

instance Encode FilePackImage where
  encode (FilePackPBM width height values) = encode $
    encodeWithSize @String "pbm"
    <> encodeWithSize width
    <> encodeWithSize height
    -- The encode instance for list already includes size info
    <> encode values

  encode (FilePackPGM width height maxValue values) = encode $
    encodeWithSize @String "pgm"
    <> encodeWithSize width
    <> encodeWithSize height
    <> encodeWithSize maxValue
    -- The encode instance for list already includes size info
    <> encode values

  encode (FilePackFOO width) = encode $
    encodeWithSize @String "foo"
    <> encodeWithSize width

instance Decode FilePackImage where
  decode = execParser $ do
    tag <- extractValue @String
    case tag of
      "pbm" ->
        FilePackPBM
        <$> extractValue
        <*> extractValue
        <*> many extractValue
      "pgm" ->
        FilePackPGM
        <$> extractValue
        <*> extractValue
        <*> extractValue
        <*> many extractValue
      otherTag ->
        fail $ "unknown image type tag: " <> otherTag
        -- Without MonadFail instance:
        -- FilePackParser $ \_ -> Left ("unknown image type tag: " <> otherTag)
