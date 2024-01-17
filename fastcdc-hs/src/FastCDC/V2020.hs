{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FastCDC.V2020
  ( FastCDCOptions (..),
    Chunk (..),
    withFastCDC,
  )
where

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

newtype FastCDC = FastCDC (ForeignPtr FFI.StreamCDC)

newFastCDC :: FunPtr FFI.ReaderFunc -> Ptr FFI.ChunkerOptions -> IO FastCDC
newFastCDC readFunPtr chunkerOptsPtr = do
  chunker <- FFI.c_chunker_new readFunPtr chunkerOptsPtr

  if chunker == nullPtr
    then error "Failed to create chunker"
    else do
      fptr <- newForeignPtr FFI.c_chunker_free chunker
      return $ FastCDC fptr

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

withFastCDC :: forall m. (MonadIO m) => FastCDCOptions -> BS.ByteString -> (Chunk -> ResourceT m ()) -> ResourceT m ()
withFastCDC options source action = do
  (_, readFunPtr) <- allocate (FFI.c_wrap_reader_func readSome) freeHaskellFunPtr

  (_, chunkerOptsPtr) <- allocate malloc free
  liftIO $
    poke chunkerOptsPtr $
      FFI.ChunkerOptions
        { FFI.minChunkSize = fromIntegral $ minChunkSize options,
          FFI.avgChunkSize = fromIntegral $ avgChunkSize options,
          FFI.maxChunkSize = fromIntegral $ maxChunkSize options
        }

  chunker <- liftIO $ newFastCDC readFunPtr chunkerOptsPtr

  processChunks chunker
  where
    readSome :: Ptr Word8 -> CSize -> IO CInt
    readSome buf size = do
      let bs = BS.take (fromIntegral size) source
      let (fp, offset, len) = BS.Internal.toForeignPtr bs
      liftIO $ withForeignPtr fp $ \p -> do
        let p' = p `plusPtr` offset
        copyBytes buf p' len
        return $ fromIntegral len

    processChunks :: FastCDC -> ResourceT m ()
    processChunks chunker = do
      mchunk <- liftIO $ nextChunk chunker
      case mchunk of
        Just chunk | len chunk /= 0 -> do
          action chunk
          processChunks chunker
        _ -> return ()

nextChunk :: (MonadIO m) => FastCDC -> m (Maybe Chunk)
nextChunk (FastCDC chunkerFptr) = liftIO $
  withForeignPtr chunkerFptr $ \chunker -> do
    chunkPtr <- FFI.c_chunker_next chunker
    if chunkPtr == nullPtr
      then return Nothing
      else do
        chunk <- peek chunkPtr
        FFI.c_chunk_free chunkPtr
        return $
          Just
            Chunk
              { hash = fromIntegral $ FFI.chash chunk,
                len = fromIntegral $ FFI.clength chunk,
                chunk = mempty -- TODO: implement
              }
