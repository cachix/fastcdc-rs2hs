module Main where

import Control.Exception (bracket)
import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import Data.Text.Encoding
import FastCDC.V2020.FFI
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable (poke)

main :: IO ()
main = do
  let chunkerOptions = ChunkerOptions {minChunkSize = 64, avgChunkSize = 256, maxChunkSize = 1024}

  withChunker "sample.txt" chunkerOptions $ \chunker -> do
    replicateM_ 3 $ inspectNextChunk chunker

withChunker :: FilePath -> ChunkerOptions -> (Ptr StreamCDC -> IO a) -> IO a
withChunker path chunkerOpts action = do
  cpath <- newCString path
  alloca $ \chunkerOptsPtr -> do
    poke chunkerOptsPtr chunkerOpts
    bracket (c_chunker_new cpath chunkerOptsPtr) c_chunker_free action

inspectNextChunk :: Ptr StreamCDC -> IO ()
inspectNextChunk chunker = do
  chunk <- nextChunk chunker
  putStrLn $ "hs chunk: " <> show chunk

  let size = fromIntegral (clength chunk)
  arr <- peekArray size (cdata chunk)
  print $ decodeUtf8 (BS.pack arr)
