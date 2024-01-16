module Main where 

import System.Environment (getArgs)
import FastCDC.V2020
import qualified Data.ByteString as BS
import GHC.IO.Handle.FD (withFile)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)
import System.IO (IOMode (ReadMode))

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

  withFile path ReadMode $ \handle -> do
    bs <- BS.hGetContents handle
    runResourceT $ withFastCDC options bs $ \chunk ->
        liftIO $ putStrLn $ "Chunk: " <> show chunk
