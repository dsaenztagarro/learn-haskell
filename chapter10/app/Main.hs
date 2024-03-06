module Main where

import Lib.Metrics.Main
import System.Environment (getArgs)

-- cabal run chapter10 -- +RTS -s -RTS "/filepath"
-- du -s -B 1
main :: IO ()
main = getArgs >>= directorySummaryWithMetrics . head
