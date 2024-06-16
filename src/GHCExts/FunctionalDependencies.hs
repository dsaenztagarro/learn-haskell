{- A functional dependency is a way for us to tell the compiler that some types
  in a multiparameter type class have a direct dependency on some other types.
-}
{-# LANGUAGE FunctionalDependencies #-}
{- It is needed FlexibleInstances extension to remove some restrictions on how
   we can create instances of type classes.

   In the context of this file, it will allow us to define an instance of a
   type class for one specific instance of a parameterized type.

   > instance ShellCommand ListDirectory [FilePath] where
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-} -- avoid lazy fields in records

module GHCExts.FunctionalDependencies where
import System.Process (readProcess)

-- cmd: Type of the command executed (i.e. ListDirectory, Grep)
-- cmdOutput: Type of the output produced by the command
--
class ShellCommand cmd cmdOutput | cmd -> cmdOutput where
  runCmd ::
    Monad m =>
    cmd ->
    (String -> [String] -> m String) ->
    m cmdOutput

newtype ListDirectory =
  ListDirectory { listDirectoryName :: FilePath }

instance ShellCommand ListDirectory [FilePath] where
  runCmd (ListDirectory dir) run =
    lines <$> run "ls" ["-1", dir] -- "-1" option list only file names

data Grep =
  Grep { grepMatch :: String, grepFiles :: [String] }

data GrepMatch = GrepMatch
  { grepMatchingFileName :: FilePath
  , grepMatchingLineNumber :: Int
  , grepMatchingLineContents :: String
  } deriving (Eq, Show)

-- Grep response format: "filename:linenumber:linecontent"
parseGrepResponse :: [String] -> [GrepMatch]
parseGrepResponse = map parseLine
  where
    parseLine responseLine =
      let
        {- span :: (a -> Bool) -> [a] -> ([a], [a])

           span, applied to a predicate p and a list xs, returns a tuple where
           first element is the longest prefix (possibly empty) of xs of
           elements that satisfy p and second element is the remainder of the list:
        -}
        (fileName, rest) = span (/= ':') responseLine
        (matchNumber, rest') = span (/= ':') $ tail rest
        contents = tail rest' -- using tail we remove ':' from the head
      in GrepMatch fileName (read matchNumber) contents

instance ShellCommand Grep [GrepMatch] where
  runCmd (Grep match grepFiles) run =
    parseGrepResponse . fixResponses . lines <$>
      run "grep" ("-n" : match : grepFiles)
    where
      {- grep command returns different output depending on number of files passed:

         grep -n pattern file1
         linenumber:linecontent

         grep -n pattern file1 file2
         filename:linenumber:linecontent
       -}
      fixResponses :: [String] -> [String]
      fixResponses responseLines =
        case grepFiles of
          [fname] -> (\l -> fname <> ":" <> l) <$> responseLines
          _multipleFiles -> responseLines

data Pipe a r b r' = Pipe a (r -> b)

instance (ShellCommand a r, ShellCommand b r') =>
  ShellCommand (Pipe a r b r') r' where
  runCmd (Pipe a mkB) run = do
    result <- runCmd a run
    runCmd (mkB result) run

grepFilesInDirectory ::
  String ->
  FilePath ->
  Pipe ListDirectory [FilePath] Grep [GrepMatch]
grepFilesInDirectory match dir =
  Pipe (ListDirectory dir) $
    Grep match . map (\fname -> dir <> "/" <> fname)

runShellCommand :: ShellCommand cmd r => cmd -> IO r
runShellCommand cmd =
  runCmd cmd (\cmdName args -> readProcess cmdName args "")
