module Data.Conduit.FastCDC.V2020
  ( fastCDC,
    FastCDCOptions (..),
    Chunk(..),
  )
where

import Conduit (ConduitT, MonadIO, MonadResource, await, liftIO, yield)
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import FastCDC.V2020 as FastCDC

fastCDC ::
  forall t m.
  (MonadIO (t m), MonadResource (t m)) =>
  FastCDCOptions ->
  ConduitT ByteString Chunk (t m) ()
fastCDC options = do
  cell <- liftIO newEmptyMVar
  leftovers <- liftIO newEmptyMVar

  let popper size = do
        lmbs <- tryTakeMVar leftovers
        case lmbs of
          Just lbs -> do
            let (xs, leftover) = BS.splitAt (fromIntegral size) lbs

            unless (BS.null leftover) $ do
              putMVar leftovers leftover

            return (Bytes xs)
          Nothing -> do
            mbs <- tryTakeMVar cell
            case mbs of
              Just bs -> do
                let (xs, leftover) = BS.splitAt (fromIntegral size) bs

                unless (BS.null leftover) $ do
                  putMVar leftovers leftover

                return (Bytes xs)
              Nothing -> do
                return Retry

  chunker <- newFastCDC options popper

  fix $ \loop -> do
    leftover <- liftIO $ tryReadMVar leftovers
    next <- liftIO $ tryReadMVar cell
    case leftover <|> next of
      -- Process leftovers first
      Just _ -> do
        mchunk <- nextChunk chunker
        mapM_ yield mchunk
        loop
      -- Fetch next input
      Nothing -> do
        mbs <- await
        case mbs of
          Nothing -> do
            liftIO $ putMVar cell mempty
            -- Drain remaining chunks
            fix $ \drain -> do
              mchunk <- nextChunk chunker
              case mchunk of
                Just c -> do
                  yield c
                  drain
                Nothing ->
                  return ()
          Just bs -> do
            liftIO $ putMVar cell bs
            loop
