module Data.Conduit.FastCDC.V2020
  ( fastCDC
  , FastCDCOptions(..)
  )
where

import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Conduit
import FastCDC.V2020
import Control.Monad.IO.Class (MonadIO, liftIO)


fastCDC :: MonadIO m => FastCDCOptions -> ConduitT ByteString Chunk (ResourceT m) ()
fastCDC options = 
    takeCE (minChunkSize options) 
  .| (awaitForever $ \bs -> lift $ withFastCDC options bs yield)