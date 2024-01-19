module Data.Conduit.FastCDC.V2020
  ( fastCDC,
    FastCDCOptions (..),
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

fastCDC :: forall t m. (MonadIO (t m), MonadResource (t m)) => FastCDCOptions -> ConduitT ByteString Chunk (t m) ()
fastCDC options = do
  cell <- liftIO newEmptyMVar
  leftovers <- liftIO newEmptyMVar

  let popper size = do
        print "pooper"
        lmbs <- tryTakeMVar leftovers
        case lmbs of
          Just lbs -> do
            let (xs, leftover) = BS.splitAt (fromIntegral size) lbs

            unless (BS.null leftover) $ do
              putStrLn $ "leftover bytes: " <> show (BS.length leftover)
              putMVar leftovers leftover

            return xs
          Nothing -> do
            mbs <- tryTakeMVar cell
            case mbs of
              Just bs -> do
                let (xs, leftover) = BS.splitAt (fromIntegral size) bs
                putStrLn $ "requested bytes: " <> show size
                putStrLn $ "got bytes: " <> show (BS.length bs)

                unless (BS.null leftover) $ do
                  putStrLn $ "leftover bytes: " <> show (BS.length leftover)
                  putMVar leftovers leftover

                return xs
              Nothing -> return mempty

  chunker <- newFastCDC' options popper

  liftIO $ putStrLn "await forever"
  fix $ \loop -> do
    mvar <- liftIO $ tryReadMVar cell <|> tryReadMVar leftovers
    case mvar of
      Nothing -> do
        mbs <- await
        case mbs of
          Nothing -> do
            liftIO $ putStrLn "chuking done"
            liftIO $ putMVar cell mempty
          Just bs -> do
            liftIO $ putStrLn "putMVar"
            liftIO $ putMVar cell bs
            loop
      Just _ -> do
        liftIO $ putStrLn "nextChunk"
        mchunk <- nextChunk chunker
        liftIO $ putStrLn "got chunk"
        case mchunk of
          Just chunk -> do
            liftIO $ putStrLn "yield chunk"
            yield chunk
          Nothing -> do
            liftIO $ putStrLn "no chunk"
        loop
