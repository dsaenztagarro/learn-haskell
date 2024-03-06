module Lib.TraverseDirectoryTree.Main where

import Lib.TraverseDirectoryTree.WithoutResultRef
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

traverseDirectory' :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory' rootPath action = do
  resultsRef <- newIORef []
  traverseDirectory rootPath $ \file -> do
    modifyIORef resultsRef (action file :)
  readIORef resultsRef

longestContents :: FilePath -> IO ByteString
longestContents rootPath = do
  contentsRef <- newIORef BS.empty
  let
    takeLongestFile a b =
      if BS.length a >= BS.length b
      then a
      else b

  traverseDirectory rootPath $ \file -> do
    contents <- BS.readFile file
    modifyIORef contentsRef (takeLongestFile contents)

  readIORef contentsRef
