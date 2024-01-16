module FastCDC.V2020
  ( FastCDCOptions(..)
  , Chunk(..)
  , withFastCDC
  ) where 

import qualified FastCDC.V2020.FFI as FFI

import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS.Internal
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable (poke)
import GHC.IO.Handle.FD (withFile)
import System.IO (IOMode (ReadMode))


data FastCDCOptions = FastCDCOptions
  { minChunkSize :: !Int,
    avgChunkSize :: !Int,
    maxChunkSize :: !Int
  }
  deriving stock (Show, Eq)

data Chunk = Chunk
  { -- | The gear value as of the end of the chunk.
    hash :: !BS.ByteString,
    -- | Length of the chunk in bytes.
    len :: !Int,
    -- | Data of the chunk.
    chunk :: !BS.ByteString
  }
  deriving stock (Show)


withFastCDC :: FilePath -> FastCDCOptions -> (Chunk -> IO ()) -> IO ()
withFastCDC path options action = do
  withFile path ReadMode $ \handle -> do
    let readSome :: Ptr Word8 -> CSize -> IO CInt
        readSome buf size = do
          bs <- BS.hGetSome handle (fromIntegral size)
          let (fp, offset, len) = BS.Internal.toForeignPtr bs
          withForeignPtr fp $ \p -> do
            let p' = p `plusPtr` offset
            copyBytes buf p' len
            return $ fromIntegral len

    readFunPtr <- FFI.c_wrap_reader_func readSome

    alloca $ \chunkerOptsPtr -> do
      poke chunkerOptsPtr $ FFI.ChunkerOptions
        { FFI.minChunkSize = fromIntegral $ minChunkSize options
        , FFI.avgChunkSize = fromIntegral $ avgChunkSize options
        , FFI.maxChunkSize = fromIntegral $ maxChunkSize options
        }
      bracket (FFI.c_chunker_new readFunPtr chunkerOptsPtr) FFI.c_chunker_free processChunks
  where
    processChunks chunker = do
      mchunk <- FFI.nextChunk chunker
      case mchunk of
        Just chunk -> unless (FFI.clength chunk == 0) $ do
            action $ Chunk
              { hash = mempty -- TODO: implement
              , len = fromIntegral $ FFI.clength chunk
              , chunk = mempty -- TODO: implement
              }
            processChunks chunker
        Nothing -> return ()