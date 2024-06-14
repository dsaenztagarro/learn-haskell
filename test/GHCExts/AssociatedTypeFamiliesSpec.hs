module GHCExts.AssociatedTypeFamiliesSpec (spec) where

import Test.Hspec
import GHCExts.AssociatedTypeFamilies
import System.FilePath ((</>))
import GHCExts.Helper (withTestDir)

spec :: Spec
spec = do
  describe "AssociatedTypeFamilies" $
    around withTestDir $ do
      it "works ListDirectory shell command" $ \testDir -> do
        result <- runShellCommand (ListDirectory testDir)
        result `shouldBe` ["FileA.hs", "FileB.hs", "FileC.hs"]

      it "works Grep shell command" $ \testDir -> do
        let match = "module"
            files = [ testDir </> "FileA.hs"
                    , testDir </> "FileB.hs"
                    , testDir </> "FileC.hs"
                    ]
        result <- runShellCommand (Grep match files)
        result `shouldBe` [
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
        result <- runShellCommand (grepFilesInDirectory "module" testDir)
        result `shouldBe` [
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
