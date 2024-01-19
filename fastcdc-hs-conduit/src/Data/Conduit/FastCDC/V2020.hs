module Data.Conduit.FastCDC.V2020
  ( fastCDC,
    FastCDCOptions (..),
    Chunk,
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
        putStrLn $ "requested: " <> show size
        case lmbs of
          Just lbs -> do
            putStrLn "processing leftovers"
            let (xs, leftover) = BS.splitAt (fromIntegral size) lbs
            putStrLn $ "split: " <> show (BS.length xs) <> " " <> show (BS.length leftover)

            unless (BS.null leftover) $ do
              putMVar leftovers leftover

            return (Bytes xs)
          Nothing -> do
            mbs <- tryTakeMVar cell
            case mbs of
              Just bs -> do
                putStrLn $ "got: " <> show (BS.length bs)
                let (xs, leftover) = BS.splitAt (fromIntegral size) bs
                putStrLn $ "split: " <> show (BS.length xs) <> " " <> show (BS.length leftover)

                unless (BS.null leftover) $ do
                  putMVar leftovers leftover

                return (Bytes xs)
              Nothing -> do
                putStrLn "cell is empty"
                return Retry

  chunker <- newFastCDC options popper

  fix $ \loop -> do
    leftover <- liftIO $ tryReadMVar leftovers
    next <- liftIO $ tryReadMVar cell
    case leftover <|> next of
      -- Process leftovers first
      Just _ -> do
        liftIO $ mapM_ (putStrLn . ("leftovers: " <>) . show . BS.length) leftover
        liftIO $ mapM_ (putStrLn . ("next: " <>) . show . BS.length) next
        mchunk <- nextChunk chunker
        -- mapM_ yield mchunk
        case mchunk of
          Just c -> do
            liftIO $ putStrLn $ "yielded chunk: " <> show (len c)
            yield c
          Nothing -> return ()
        liftIO $ mapM (print . len) mchunk
        loop
      -- Fetch next input
      Nothing -> do
        liftIO $ putStrLn "awaiting"
        mbs <- await
        case mbs of
          Nothing -> do
            liftIO $ putMVar cell mempty
            fix $ \floop -> do
              mchunk <- nextChunk chunker
              -- mapM_ yield mchunk
              case mchunk of
                Just c -> do
                  liftIO $ putStrLn $ "yielded final chunk: " <> show (len c)
                  yield c
                  floop
                Nothing -> do
                  liftIO $ putStrLn "done"
                  return ()
          Just bs -> do
            liftIO $ putStrLn "put stuff"
            liftIO $ putMVar cell bs
            loop
