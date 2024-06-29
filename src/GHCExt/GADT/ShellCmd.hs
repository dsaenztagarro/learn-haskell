{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

-- Composing individual parts of shell commands in a type safe way
module GHCExt.GADT.ShellCmd where
import Control.Monad.Catch (catchIOError) -- part of `exceptions` package
import System.Process (readProcess)
import System.FilePath ((</>))

newtype ProgName = ProgName { getProgName :: FilePath }
newtype ProgArgs = ProgArgs { getProgArgs :: [String] }

data ShellCmd a b where
  RunCommand ::
    ProgName -> (a -> ProgArgs) -> (a -> String -> b) -> ShellCmd a b
  Pipe :: ShellCmd a b -> ShellCmd b c -> ShellCmd a c
  Xargs :: ShellCmd a b -> ShellCmd [a] [b]
  MapOut :: (b -> c) -> ShellCmd b c

data GrepMatch = GrepMatch
  { grepMatchingFileName :: FilePath
  , grepMatchingLineNumber :: Int
  , grepMatchingLineContents :: String
  } deriving (Eq, Show)

grep :: String -> ShellCmd FilePath [GrepMatch]
grep matchGlob =
  RunCommand (ProgName "grep") makeArgs parseLines
  where
    makeArgs fileName = ProgArgs $ "-n" : matchGlob : [fileName]
    parseLines fileName = map (parseResponse fileName) . lines
    parseResponse fileName responseLine =
      let (matchNumber, contents) = span (/= ':') responseLine
      in GrepMatch fileName (read matchNumber) (tail contents)

listDirectory :: ShellCmd FilePath [FilePath]
listDirectory =
  RunCommand (ProgName "ls") makeArgs parseResponse
  where
    makeArgs filePath = ProgArgs ["-1", filePath]
    parseResponse filePath =
      map (filePath </>) . lines

printFile :: ShellCmd FilePath String
printFile = 
  RunCommand (ProgName "cat") makeArgs parseResponse
  where
    makeArgs filePath = ProgArgs [filePath]
    parseResponse filePath output = output

runShellCmd :: ShellCmd a b -> a -> IO b
runShellCmd cmd input =
  case cmd of
    RunCommand (ProgName execName) mkArgs parseOut ->
      parseOut input <$> catchIOError processOut (const $ pure "")
      where
        processOut = readProcess execName (getProgArgs $ mkArgs input) ""
    Pipe inputCmd out -> runShellCmd inputCmd input >>= runShellCmd out
    Xargs inputCmd -> mapM (runShellCmd inputCmd) input
    MapOut mapF -> pure $ mapF input
