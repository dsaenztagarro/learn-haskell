{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Module      : Apps.FilePack.Encoder3
-- Stage       : 08-Apps  (see docs/ROADMAP.md)
-- Source      : Effective Haskell — Building applications chapter (FilePack)
-- Prereqs     : Apps.FilePack.Encoder2,
--               Exts.Types.ExistentialQuantification.WithTypeClassConstraint
--
-- == Concept
-- Third iteration: 'Packable' is an existential (@forall a. Encode a@)
-- so a 'FilePack' can mix files of different value-shapes while still
-- knowing how to serialize each one. Mirrors the design of the
-- @binary@ package's @Get@/@Put@ pair on a smaller scale.
module Apps.FilePack.Encoder3 where

import Apps.FilePack.Util

data Packable = forall a. Encode a => Packable { getPackable :: FileData a }

newtype FilePack = FilePack [Packable]

instance Encode Packable where
  encode (Packable p) = encode p

instance Encode FilePack where
  encode (FilePack p) = encode p

---------------------------------
-- Functions to build a FilePack
---------------------------------

addFileDataToPack :: Encode a => FileData a -> FilePack -> FilePack
addFileDataToPack a (FilePack as) = FilePack $ (Packable a) : as

infixr 6 .:
(.:) :: (Encode a) => FileData a -> FilePack -> FilePack
(.:) = addFileDataToPack

emptyFilePack :: FilePack
emptyFilePack = FilePack []
