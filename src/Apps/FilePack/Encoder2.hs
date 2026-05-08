{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Apps.FilePack.Encoder2
-- Stage       : 08-Apps  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Building applications chapter (FilePack)
-- Prereqs     : Apps.FilePack.Encoder1
--
-- == Concept
-- Second iteration: replace 'Show'/@readEither@ with a tag byte +
-- length-prefixed payload, encoded by hand. Introduces 'Encode' /
-- 'Decode' classes (in 'Apps.FilePack.Util') so each field type knows
-- how to serialize itself.
module Apps.FilePack.Encoder2 where

import Apps.FilePack.Util
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word
import Text.Read (readEither)

newtype FilePack a =
  FilePack { getPackedFiles :: [FileData a] } deriving (Eq, Read, Show)

