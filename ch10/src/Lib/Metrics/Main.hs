module Lib.Metrics.Main where

import Data.Foldable (for_)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Lib.Metrics
import Lib.TraverseDirectoryTree.WithMetrics
import Text.Printf (printf)

directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
  metrics <- newMetrics
  histogramRef <- newIORef (Map.empty :: Map.Map Char Int)
  traverseDirectory metrics root $ \file -> do
    putStrLn $ file <> ":"
    contents <- timeFunction metrics "TextIO.readFile" $
                  TextIO.readFile file
    timeFunction metrics "wordcount" $
      let wordCount = length $ Text.words contents
      in putStrLn $ "   word count: " <> show wordCount

    timeFunction metrics "histogram" $ do
      oldHistogram <- readIORef histogramRef
      let
        addCharToHistogram histogram letter =
          Map.insertWith (+) letter 1 histogram
        newHistogram =
          Text.foldl' addCharToHistogram oldHistogram contents
      writeIORef histogramRef newHistogram

  timeFunction metrics "print histogram" $ do
    histogram <- readIORef histogramRef
    putStrLn "Histogram Data:"
    for_ (Map.toList histogram) $ \(letter, count) ->
      putStrLn $ printf "     %c: %d" letter count

    displayMetrics metrics

  displayMetrics metrics


