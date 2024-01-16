module Data.Conduit.FastCDC.V2020
  ( fastCDC,
  )
where

import Data.ByteString (ByteString)
import Data.Conduit
import FastCDC.V2020

fastCDC :: (Monad m) => FastCDCOptions -> ConduitT ByteString ByteString m ()
fastCDC _ = undefined
