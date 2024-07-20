{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Base.Monad.MonadTransSpec where

import Test.Hspec
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Base.Monad.MonadTrans
import Base.Monad.StateT
import Base.Monad.ExceptT

{-
data Archive = Archive
  { archiveName :: Text
  , archiveFiles :: [ArchivedFile]
  } deriving stock (Show)

data ArchivedFile = ArchivedFile
  { archivedFileName :: Text
  , archivedFileContents :: ByteString
  } deriving stock (Show)

newtype Archiver a = Archiver
  { unArchiver :: StateT Text (ExceptT Text IO) a }
  deriving newtype (Functor, Applicative, Monad, Alternative)

runArchiver :: Text -> Archiver a -> IO (Either Text a)
runArchiver inputText archiver =
  runExceptT $ evalStateT (unArchiver archiver) inputText

parseChar :: Archiver Char
parseChar = do
  parseText <- Archiver get
  case Text.uncons parseText of
    Nothing ->
      Archiver . liftStateT . throwError $ "end of input"
    Just (c, rest) -> do
      Archiver (put rest)
      pure c

readArchiveContents :: FilePath -> Archiver ByteString
readArchiveContents =
  Archive . lift . lift . BS.readFile
-}

spec :: Spec
spec = do
    it "works" $ do
      1 `shouldBe` 1
