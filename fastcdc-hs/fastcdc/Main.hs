module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import FastCDC.V2020
import GHC.IO.Handle.FD (withFile)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode))

options :: FastCDCOptions
options =
  FastCDCOptions
    { minChunkSize = 8 * 1024,
      avgChunkSize = 16 * 1024,
      maxChunkSize = 32 * 1024
    }

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
    [path] -> return path
    _otherwise -> error "Usage: fastcdc <file>"

  withFile path ReadMode $ \handle -> do
    let popper = BS.hGetSome handle
    runResourceT $ withFastCDC options popper $ \c ->
      liftIO $ putStrLn $ "Chunk: " <> "hash=" <> show (hash c) <> " size=" <> show (len c)
