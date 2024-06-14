module GHCExts.Helper where

import Test.Hspec
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (getTemporaryDirectory, makeAbsolute, createDirectory,
                         removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (openFile, hPutStr, IOMode(WriteMode), hClose)

{-
type Spec = SpecWith ()

type ActionWith a = a -> IO ()

around :: (ActionWith a -> IO ()) -> SpecWith a -> Spec

bracket :: IO a         computation to run first ("adquire resource")
        -> (a -> IO b)  computation to run last ("release resource")
        -> (a -> IO c)  computation to run in-between
        IO c
-}

-- TODO: refactor to withTestDir withFiles using `nest`
-- https://wiki.haskell.org/Bracket_pattern

withTestDir :: ActionWith FilePath -> IO ()
withTestDir = bracket (createTestDir >>= populateTestDir) deleteTestDir
  where
    createTestDir :: IO FilePath
    createTestDir = do
      tmpDir <- makeAbsolute =<< getTemporaryDirectory
      milliseconds <- show <$> getPOSIXTime
      let testDir = tmpDir </> "hspec-" <> milliseconds
      createDirectory testDir
      return testDir

    populateTestDir :: FilePath -> IO FilePath
    populateTestDir testDir = do
      let contents = [ ("FileA.hs", "module FileA where")
                     , ("FileB.hs", "class Outputable a where")
                     , ("FileC.hs", "import FileA\nmodule FileC where")
                     ]

      forM_ contents $ \(filename, content) -> do
        let path = testDir </> filename
        handle <- openFile path WriteMode
        hPutStr handle content
        hClose handle

      return testDir

    deleteTestDir :: FilePath -> IO ()
    deleteTestDir = removeDirectoryRecursive
