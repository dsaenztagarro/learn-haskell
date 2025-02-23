module Exts.Kinds.AssociatedTypeFamily.ShellCmdSpec (spec) where

import Test.Hspec
import Exts.Kinds.AssociatedTypeFamily.ShellCmd
import System.FilePath ((</>))
import Exts.Helper (withTestDir)

spec :: Spec
spec = do
  describe "AssociatedTypeFamily" $
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
