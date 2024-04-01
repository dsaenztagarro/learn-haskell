{-# LANGUAGE TypeApplications #-}
module MonadicParser where

import ApplicativeParser
import Control.Applicative
import Data.Word (Word32)
import Encoding

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
  deriving (Eq, Show)

instance Encode FilePackImage where
  encode (FilePackPBM width height values) = encode $
    encodeWithSize @String "pbm"
    <> encodeWithSize width
    <> encodeWithSize height
    <> encodeWithSize values

  encode (FilePackPGM width height maxValue values) = encode $
    encodeWithSize @String "pgm"
    <> encodeWithSize width
    <> encodeWithSize height
    <> encodeWithSize maxValue
    <> encodeWithSize values

instance Decode FilePackImage where
  decode = execParser $ do
    tag <- extractValue @String
    case tag of
      "pbm" ->
        FilePackPBM
        <$> extractValue
        <*> extractValue
        <*> extractValue
      "pgm" ->
        FilePackPGM
        <$> extractValue
        <*> extractValue
        <*> extractValue
        <*> many extractValue
      otherTag ->
        fail $ "unknown image type tag: " <> otherTag
        -- Without MonadFail instance:
        -- FilePackParser $ \_ -> Left "unknown image type tag: " <> otherTag
