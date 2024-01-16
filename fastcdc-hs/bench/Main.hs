module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import FastCDC.V2020
import GHC.IO.Handle.FD (withFile)
import System.IO (IOMode (ReadMode))

options :: FastCDCOptions
options = FastCDCOptions
    { minChunkSize = 8 * 1024
    , avgChunkSize = 16 * 1024
    , maxChunkSize = 32 * 1024
    }

main :: IO ()
main = defaultMain
  [ bgroup "FastCDC.V2020"
    [ bench "SekienAkashita.jpg" $ env (withFile "bench/SekienAkashita.jpg" ReadMode BS.hGetContents)
        $ \bs -> whnfIO $ withFastCDC options bs (const $ return ())
    ]
  ]