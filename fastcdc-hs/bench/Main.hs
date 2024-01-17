{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.DeepSeq (NFData (..))
import Criterion.Main
import qualified Data.ByteString as BS
import FastCDC.V2020
import System.IO (Handle, IOMode (ReadMode), hClose, openFile)

options :: FastCDCOptions
options =
  FastCDCOptions
    { minChunkSize = 8 * 1024,
      avgChunkSize = 16 * 1024,
      maxChunkSize = 32 * 1024
    }

instance NFData Handle where
  rnf !_ = ()

main :: IO ()
main =
  defaultMain
    [ bgroup
        "FastCDC.V2020"
        [ bench "SekienAkashita.jpg ENV" $
            perRunEnvWithCleanup (openFile "bench/SekienAkashita.jpg" ReadMode) hClose $ \handle -> do
              let popper = BS.hGetSome handle
              runFastCDC options popper $ \c -> BS.length (chunk c) `seq` return ()
        ]
    ]
