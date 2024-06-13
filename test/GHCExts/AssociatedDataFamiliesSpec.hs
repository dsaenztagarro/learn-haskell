module GHCExts.AssociatedDataFamiliesSpec (spec) where

import Test.Hspec
import GHCExts.AssociatedDataFamilies
import Control.Exception (bracket)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (getTemporaryDirectory, makeAbsolute, createDirectory,
                         removeDirectoryRecursive)
import System.FilePath ((</>))

{-
type Spec = SpecWith ()

around_ :: (IO () -> IO ()) -> SpecWith a -> SpecWith a

bracket :: IO a         computation to run first ("adquire resource")
        -> (a -> IO b)  computation to run last ("release resource")
        -> (a -> IO c)  computation to run in-between
        IO c
-}

-- TODO: refactor to withTestDir withFiles using `nest`
-- https://wiki.haskell.org/Bracket_pattern

withTestDir :: IO () -> IO ()
withTestDir action =
  bracket createTestDir deleteTestDir (const action)

createTestDir :: IO FilePath
createTestDir = do
  tmpDir <- makeAbsolute =<< getTemporaryDirectory
  milliseconds <- show <$> getPOSIXTime
  let testDir = tmpDir </> milliseconds
  createDirectory testDir
  return testDir

deleteTestDir :: FilePath -> IO ()
deleteTestDir = removeDirectoryRecursive

spec :: Spec
spec = do
  describe "AssociatedDataFamilies" $ around_ withTestDir $ do
    it "works ListDirectory shell command" $ do
      let dir = "/Users/dsaenz/Code/effective-haskell/test/dummy/src/"
      directoryListing <- runShellCommand (ListDirectory dir)
      filenamesInListing directoryListing `shouldBe` ["FileA.hs", "FileB.hs", "FileC.hs"]

    it "works Grep shell command" $ do
      let match = "module"
          files = [
            "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileA.hs",
            "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileB.hs",
            "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileC.hs"]
      ListOfGrepMatches matches <- runShellCommand (Grep match files)
      matches `shouldBe` [
          GrepMatch {
            grepMatchingFileName = "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileA.hs",
            grepMatchingLineNumber = 1,
            grepMatchingLineContents = "module MyFile where"
          },
          GrepMatch {
            grepMatchingFileName = "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileC.hs",
            grepMatchingLineNumber = 1,
            grepMatchingLineContents = "module FileC where"
          }
        ]

    it "works grepFilesInDirectory shell command" $ do
      let dir = "/Users/dsaenz/Code/effective-haskell/test/dummy/src"
      PipeOutput (ListOfGrepMatches matches) <- runShellCommand (grepFilesInDirectory "module" dir)
      matches `shouldBe` [
          GrepMatch {
            grepMatchingFileName = "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileA.hs",
            grepMatchingLineNumber = 1,
            grepMatchingLineContents = "module MyFile where"
          },
          GrepMatch {
            grepMatchingFileName = "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileC.hs",
            grepMatchingLineNumber = 1,
            grepMatchingLineContents = "module FileC where"
          }
        ]
