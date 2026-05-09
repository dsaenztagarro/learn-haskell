{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : Apps.FilePack.MonadicParser
-- Stage       : 08-Apps  (see docs/ROADMAP.md)
-- Source      : EH:ch? parsers
-- Prereqs     : Apps.FilePack.ApplicativeParser
--
-- == Concept
-- The same parser machinery as 'Apps.FilePack.ApplicativeParser' but
-- with a 'Monad' instance: later parses can branch on earlier
-- results. The cost is that you can no longer statically analyze
-- where the parser will look. Reading both modules side-by-side is
-- the fastest way to internalise the Applicative-vs-Monad parser
-- trade-off.
module Apps.FilePack.MonadicParser where

import Apps.FilePack.ApplicativeParser (FilePack, FilePackParser(..), extractValue, execParser)
import Apps.FilePack.Util
import Prelude hiding (MonadFail, fail)
import Control.Applicative
import Data.Word (Word32)
import Libs.Mtl.Control.Monad.MonadFail

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
    -- Prefixing encoded data with a String tag, to differentiate between
    -- different constructors in a sum type is a common PATTERN
    -- Original example uses OverloadedStrings, and in that case is needed
    -- TypeApplications to avoid Ambiguous type variable
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
