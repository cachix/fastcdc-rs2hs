module Data.Conduit.FastCDC.V2020
  ( fastCDC
  , FastCDCOptions(..)
  )
where

import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Conduit
import FastCDC.V2020


fastCDC :: MonadUnliftIO m => FastCDCOptions -> ConduitT ByteString Chunk m ()
fastCDC options = 
    takeCE (minChunkSize options) 
  .| (awaitForever $ \bs -> withFastCDC options bs yield)
