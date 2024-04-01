{-# LANGUAGE TypeApplications #-}
module Lib.TraverseDirectoryTree.WithMetrics where

import Control.Exception (IOException, handle)
import Control.Monad (join, void, when)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import qualified Data.Set as Set (empty, insert, member)
import Lib.Metrics
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import Text.Printf (printf)

dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s = take (length s - length suffix) s
  | otherwise = s

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
           (True, False) -> FileTypeDirectory
           (False, True) -> FileTypeRegularFile
           _otherwise -> FileTypeOther

-- NOTE: added metrics param
traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO()) -> IO ()
traverseDirectory metrics rootPath action = do
  seenRef <- newIORef Set.empty
  let
    haveSeenDirectory canonicalPath =
      Set.member canonicalPath <$> readIORef seenRef

    addDirectoryToSeen canonicalPath =
      modifyIORef seenRef $ Set.insert canonicalPath

    -- NOTE: tick failure
    handler ex = print ex >> tickFailure metrics

    traverseSubdirectory subdirPath = do
      -- NOTE: time function
      timeFunction metrics "traverseSubdirectory" $ do
        contents <- listDirectory subdirPath
        for_ contents $ \file' ->
          handle @IOException handler $ do
            let file = subdirPath <> "/" <> file'
            canonicalPath <- canonicalizePath file
            classification <- classifyFile canonicalPath
            result <- case classification of
              FileTypeOther -> pure ()
              FileTypeRegularFile ->
                action file
              FileTypeDirectory -> do
                alreadyProcessed <- haveSeenDirectory file
                when (not alreadyProcessed) $ do
                  addDirectoryToSeen file
                  traverseSubdirectory file
            -- NOTE: tick success
            tickSuccess metrics
            pure result

  traverseSubdirectory (dropSuffix "/" rootPath)

-- traverseDirectory "/tmp/test" countBytes
-- :: IO [IO (FilePath, Integer)]
-- fmap sequence $ traverseDirectory "/tmp/test" countBytes
-- :: IO (IO [(FilePath, Integer)])
-- join . fmap sequence $ traverseDirectory "/tmp/test" countBytes
-- :: IO [(FilePath, Integer)]
countBytes :: FilePath -> IO (FilePath, Integer)
countBytes path = do
  bytes <- fromIntegral . BS.length <$> BS.readFile path
  pure (path, bytes)
