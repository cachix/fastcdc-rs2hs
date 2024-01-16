module Main where

import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS.Internal
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

  let chunkerOptions = ChunkerOptions
        { minChunkSize = 8 * 1024 
        , avgChunkSize = 16 * 1024
        , maxChunkSize = 32 * 1024
        }

  withChunker path chunkerOptions $ \chunk ->
    putStrLn $ "Chunk: " <> show chunk

withChunker :: FilePath -> ChunkerOptions -> (ChunkData -> IO ()) -> IO ()
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
      bracket (c_chunker_new readFunPtr chunkerOptsPtr) c_chunker_free processChunks
  where
    processChunks chunker = do
      mchunk <- nextChunk chunker
      case mchunk of
        Just chunk -> unless (clength chunk == 0) $ do
            action chunk
            processChunks chunker
        Nothing -> return ()