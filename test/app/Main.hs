module Main where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS.Internal
import Data.Text.Encoding
import Data.Word
import FastCDC.V2020.FFI
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable (poke)
import GHC.IO.Handle.FD (withFile)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode))

main :: IO ()
main = do
  path <-
    getArgs >>= \case
      [path] -> return path
      _otherwise -> error "Usage: fastcdc-sample <file>"

  let chunkerOptions = ChunkerOptions {minChunkSize = 512, avgChunkSize = 1024, maxChunkSize = 4096}

  withChunker path chunkerOptions $ \chunker ->
    fix $ \loop -> do
      more <- inspectNextChunk chunker
      when more loop

withChunker :: FilePath -> ChunkerOptions -> (Ptr StreamCDC -> IO a) -> IO a
withChunker path chunkerOpts action = do
  withFile path ReadMode $ \handle -> do
    let readSome :: Ptr Word8 -> CSize -> IO CInt
        readSome buf size = do
          bs <- BS.hGetSome handle (fromIntegral size)
          let (fp, offset, len) = BS.Internal.toForeignPtr bs
          withForeignPtr fp $ \p -> do
            let p' = p `plusPtr` offset
            copyBytes buf p' len
            return $ fromIntegral len

    readFunPtr <- c_wrap_reader_func readSome

    alloca $ \chunkerOptsPtr -> do
      poke chunkerOptsPtr chunkerOpts
      bracket (c_chunker_new readFunPtr chunkerOptsPtr) c_chunker_free action

inspectNextChunk :: Ptr StreamCDC -> IO Bool
inspectNextChunk chunker = do
  mchunk <- nextChunk chunker

  case mchunk of
    Nothing -> return False
    Just chunk -> do
      putStrLn $ "hs chunk: " <> show chunk

      let size = fromIntegral (clength chunk)
      if size == 0
        then return False
        else do
          printChunk size (cdata chunk)
          return True
  where
    printChunk size cd = do
      arr <- peekArray size cd
      print $ decodeUtf8 (BS.pack arr)
