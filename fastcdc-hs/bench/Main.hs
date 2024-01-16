module Main where

import Criterion.Main
import FastCDC.V2020

options :: FastCDCOptions
options = FastCDCOptions
    { minChunkSize = 8 * 1024
    , avgChunkSize = 16 * 1024
    , maxChunkSize = 32 * 1024
    }

main :: IO ()
main = defaultMain
  [ bgroup "FastCDC.V2020"
    [ bench "SekienAkashita.jpg" $ 
        -- TODO: read the whole file into memory and pass it to withFastCDC
        whnfIO $ withFastCDC "bench/SekienAkashita.jpg" options (const $ return ())
    ]
  ]