module GHCExts.FunctionalDependenciesSpec (spec) where

import Test.Hspec
import GHCExts.FunctionalDependencies
import System.Directory (getTemporaryDirectory, makeAbsolute)

-- TODO: extract hook to create temp dir and files with content

spec :: Spec
spec = do
  -- describe "FunctionalDependencies" $ around_ withTempDir do
  describe "FunctionalDependencies" $ do
    it "works ListDirectory shell command" $ do
      let dir = "/Users/dsaenz/Code/effective-haskell/test/dummy/src/"
      result <- runShellCommand (ListDirectory dir)
      result `shouldBe` ["FileA.hs", "FileB.hs", "FileC.hs"]

    it "works Grep shell command" $ do
      let match = "module"
          files = [
            "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileA.hs",
            "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileB.hs",
            "/Users/dsaenz/Code/effective-haskell/test/dummy/src/FileC.hs"]
      result <- runShellCommand (Grep match files)
      result `shouldBe` [
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
      result <- runShellCommand (grepFilesInDirectory "module" dir)
      result `shouldBe` [
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
