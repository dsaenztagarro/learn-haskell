module GHCExt.Kinds.AssociatedDataFamily.ShellCmdSpec (spec) where

import Test.Hspec
import GHCExt.Kinds.AssociatedDataFamily.ShellCmd
import GHCExt.Helper (withTestDir)
import System.FilePath ((</>))

spec :: Spec
spec = do
  describe "AssociatedDataFamilies" $ around withTestDir $ do
    it "works ListDirectory shell command" $ \testDir -> do
      directoryListing <- runShellCommand (ListDirectory testDir)
      filenamesInListing directoryListing `shouldBe` ["FileA.hs", "FileB.hs", "FileC.hs"]

    it "works Grep shell command" $ \testDir -> do
      let match = "module"
          files = [ testDir </> "FileA.hs"
                  , testDir </> "FileB.hs"
                  , testDir </> "FileC.hs"
                  ]
      ListOfGrepMatches matches <- runShellCommand (Grep match files)
      matches `shouldBe` [
          GrepMatch {
            grepMatchingFileName = testDir </> "FileA.hs",
            grepMatchingLineNumber = 1,
            grepMatchingLineContents = "module FileA where"
          },
          GrepMatch {
            grepMatchingFileName = testDir </> "FileC.hs",
            grepMatchingLineNumber = 2,
            grepMatchingLineContents = "module FileC where"
          }
        ]

    it "works grepFilesInDirectory shell command" $ \testDir -> do
      PipeOutput (ListOfGrepMatches matches) <- runShellCommand (grepFilesInDirectory "module" testDir)
      matches `shouldBe` [
          GrepMatch {
            grepMatchingFileName = testDir </> "FileA.hs",
            grepMatchingLineNumber = 1,
            grepMatchingLineContents = "module FileA where"
          },
          GrepMatch {
            grepMatchingFileName = testDir </> "FileC.hs",
            grepMatchingLineNumber = 2,
            grepMatchingLineContents = "module FileC where"
          }
        ]
