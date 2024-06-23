{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-} -- avoid lazy fields in records
{-# LANGUAGE RecordWildCards #-}

module GHCExts.Kinds.AssociatedDataFamily where
import Data.Kind -- needed by associated type family to reference "Type"
import System.Process (readProcess)

{- See how it changes from using FunctionalDependencies
   |
   | class ShellCommand cmd cmdOutput | cmd -> cmdOutput where
   |
   v
-}
class ShellCommand cmd where
  {- A data family works like a type family, except that every data family
     instance defines a brand new type, rather than creating an alias for some
     existing type.
     |
     | We use the `data` keyword instead of `type`
     |
     v
  -}
  data ShellOutput cmd :: Type

  runCmd ::
    Monad m =>
    cmd ->
    (String -> [String] -> m String) ->
    m (ShellOutput cmd)

newtype ListDirectory =
  ListDirectory { listDirectoryName :: FilePath }

instance ShellCommand ListDirectory where
  {- DATA FAMILY defines BRAND NEW TYPE `DirectoryListing`, by defining a new
     record
  -}
  data ShellOutput ListDirectory =
    DirectoryListing { containingDirectory :: FilePath
                     , filenamesInListing :: [FilePath]
                     } deriving (Show, Eq)

  runCmd (ListDirectory dir) run =
    -- `DirectoryListing dir .` means to be a partially applied data constructor
    DirectoryListing dir . lines <$> run "ls" ["-1", dir] -- "-1" option list only file names

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

instance ShellCommand Grep where
  {- We can use `newtype` with data families to get the extra type safety
     benefits of a data family without any performance overhead.
  -}
  newtype ShellOutput Grep =
    ListOfGrepMatches { getListOfGrepMatches :: [GrepMatch] }

  runCmd Grep{..} run =
    ListOfGrepMatches . parseGrepResponse . fixResponses . lines <$> run "grep" grepArgs
    where
      grepArgs = "-n" : grepMatch : grepFiles
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

data Pipe a b = Pipe a (ShellOutput a -> b)

instance (ShellCommand a, ShellCommand b) =>
  ShellCommand (Pipe a b) where
  data ShellOutput (Pipe a b) = PipeOutput (ShellOutput b)

  runCmd (Pipe a mkB) run = do
    result <- runCmd a run
    PipeOutput <$> runCmd (mkB result) run

grepFilesInDirectory ::
  String ->
  FilePath ->
  Pipe ListDirectory Grep
grepFilesInDirectory match dir =
  Pipe (ListDirectory dir) $
    \result ->
      Grep match (map (\fname -> dir <> "/" <> fname) (filenamesInListing result))

runShellCommand :: ShellCommand cmd => cmd -> IO (ShellOutput cmd)
runShellCommand cmd =
  runCmd cmd (\cmdName args -> readProcess cmdName args "")
