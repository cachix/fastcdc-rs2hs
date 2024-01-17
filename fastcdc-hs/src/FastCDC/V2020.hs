module FastCDC.V2020
  ( FastCDCOptions (..),
    Chunk (..),
    withFastCDC,
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS.Internal
import Data.Word
import qualified FastCDC.V2020.FFI as FFI
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data FastCDCOptions = FastCDCOptions
  { minChunkSize :: !Int,
    avgChunkSize :: !Int,
    maxChunkSize :: !Int
  }
  deriving stock (Show, Eq)

data Chunk = Chunk
  { -- | The gear value as of the end of the chunk.
    -- TODO: we may want to newtype this to provide useful functions over the gear hash.
    hash :: !Word64,
    -- | Length of the chunk in bytes.
    len :: !Int,
    -- | Data of the chunk.
    chunk :: !BS.ByteString
  }
  deriving stock (Show)

withFastCDC :: (MonadIO m) => FastCDCOptions -> BS.ByteString -> (Chunk -> ResourceT m ()) -> ResourceT m ()
withFastCDC options source action = do
  readFunPtr <- liftIO $ FFI.c_wrap_reader_func readSome

  (_, chunkerOptsPtr) <-
    allocate
      (malloc :: IO (Ptr FFI.ChunkerOptions))
      free
  liftIO $ do
    poke chunkerOptsPtr $
      FFI.ChunkerOptions
        { FFI.minChunkSize = fromIntegral $ minChunkSize options,
          FFI.avgChunkSize = fromIntegral $ avgChunkSize options,
          FFI.maxChunkSize = fromIntegral $ maxChunkSize options
        }
  (_, chunker) <-
    allocate
      (FFI.c_chunker_new readFunPtr chunkerOptsPtr)
      FFI.c_chunker_free
  processChunks chunker
  where
    readSome :: (MonadIO m) => Ptr Word8 -> CSize -> m CInt
    readSome buf size = do
      let bs = BS.take (fromIntegral size) source
      let (fp, offset, len) = BS.Internal.toForeignPtr bs
      liftIO $ withForeignPtr fp $ \p -> do
        let p' = p `plusPtr` offset
        copyBytes buf p' len
        return $ fromIntegral len

    -- processChunks :: Ptr FFI.StreamCDC -> m ()
    processChunks chunker = do
      mchunk <- liftIO $ FFI.nextChunk chunker
      case mchunk of
        Just chunk | FFI.clength chunk /= 0 -> do
          action $
            Chunk
              { hash = fromIntegral $ FFI.chash chunk,
                len = fromIntegral $ FFI.clength chunk,
                chunk = mempty -- TODO: implement
              }
          processChunks chunker
        _ -> return ()
