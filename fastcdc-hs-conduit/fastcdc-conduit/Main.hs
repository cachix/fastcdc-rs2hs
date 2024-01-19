module Main where

import Conduit
import Data.Conduit.FastCDC.V2020
import FastCDC.V2020 as FastCDC
import System.CPUTime
import System.Directory (getFileSize)
import System.Environment (getArgs)

main :: IO ()
main = do
  path <-
    getArgs >>= \args -> case args of
      [path] -> return path
      _ -> error "Usage: fastcdc-conduit <path>"

  -- Compute the size of the file
  fsSize <- getFileSize path

  start <- getCPUTime

  size <-
    runConduitRes $
      sourceFile path
        .| fastCDC (FastCDCOptions (4 * 1024) (16 * 1024) (64 * 1024))
        -- Fetch the len of each Chunk and sum them up
        .| foldlC (\acc c -> acc + len c) 0

  end <- getCPUTime
  let duration :: Double = fromIntegral (end - start) / (10 ^ (12 :: Integer))

  putStrLn $
    unlines
      [ "File size: " <> show fsSize <> " bytes",
        "Sum of chunks: " <> show size <> " bytes",
        "Duration: " <> show duration <> " seconds"
      ]
