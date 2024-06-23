module GHCExts.GADTs.ComposabilitySpec (spec) where

import Test.Hspec
import GHCExts.GADTs.Composability
import GHCExts.Helper (withTestDir)
import System.FilePath ((</>))

spec :: Spec
spec = do
  describe "runShellCmd" $ around withTestDir $ do
    it "runs ListDirectory command" $ \testDir -> do
      fileNames <- runShellCmd listDirectory testDir
      fileNames `shouldBe`
        [ testDir </> "FileA.hs"
        , testDir </> "FileB.hs"
        , testDir </> "FileC.hs"
        ]

    it "runs Grep command" $ \testDir -> do
      let match = "module"
          fileName = testDir </> "FileC.hs"
      matches <- runShellCmd (grep match) fileName
      matches `shouldBe`
        [ GrepMatch {
            grepMatchingFileName = testDir </> "FileC.hs",
            grepMatchingLineNumber = 2,
            grepMatchingLineContents = "module FileC where"
          }
        ]

    it "runs Pipe command" $ \testDir -> do
      let pipeCmd = Pipe listDirectory (Xargs $ grep "module")
      matches <- runShellCmd pipeCmd testDir
      matches `shouldBe` [
          [ GrepMatch {
              grepMatchingFileName = testDir </> "FileA.hs",
              grepMatchingLineNumber = 1,
              grepMatchingLineContents = "module FileA where"
            }
          ],
          [],
          [ GrepMatch {
              grepMatchingFileName = testDir </> "FileC.hs",
              grepMatchingLineNumber = 2,
              grepMatchingLineContents = "module FileC where"
            }
          ]
        ]
