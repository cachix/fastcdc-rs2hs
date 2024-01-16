module Data.Conduit.FastCDC.V2020
  ( fastCDC,
  )
where

import Data.ByteString (ByteString)
import Data.Conduit
import FastCDC.V2020.FFI

fastCDC :: (Monad m) => ChunkerOptions -> ConduitT ByteString ByteString m ()
fastCDC chunkerOpts = undefined
