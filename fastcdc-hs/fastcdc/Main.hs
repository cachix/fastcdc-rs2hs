module Main where 

import System.Environment (getArgs)
import FastCDC.V2020

options :: FastCDCOptions
options = FastCDCOptions
    { minChunkSize = 8 * 1024 
    , avgChunkSize = 16 * 1024
    , maxChunkSize = 32 * 1024
    }

main :: IO ()
main = do
  args <- getArgs 
  path <- case args of
    [path] -> return path
    _otherwise -> error "Usage: fastcdc <file>"

  withFastCDC path options $ \chunk ->
    putStrLn $ "Chunk: " <> show chunk
