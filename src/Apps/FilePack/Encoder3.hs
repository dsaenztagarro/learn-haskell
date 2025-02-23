{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
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
