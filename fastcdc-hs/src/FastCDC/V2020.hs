{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FastCDC.V2020
  ( FastCDCOptions (..),
    Chunk (..),
    withFastCDC,
    runFastCDC,
    newFastCDC',
    nextChunk,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)
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

runFastCDC :: (MonadUnliftIO m) => FastCDCOptions -> (Int -> IO BS.ByteString) -> (Chunk -> ResourceT m ()) -> m ()
runFastCDC options source action = runResourceT $ withFastCDC options source action

newFastCDC' options popper = do
  (_, readFunPtr) <-
    allocate (FFI.c_wrap_reader_func readSome) freeHaskellFunPtr

  (_, chunkerOptsPtr) <- allocate malloc free
  liftIO $
    poke chunkerOptsPtr $
      FFI.ChunkerOptions
        { FFI.minChunkSize = fromIntegral $ minChunkSize options,
          FFI.avgChunkSize = fromIntegral $ avgChunkSize options,
          FFI.maxChunkSize = fromIntegral $ maxChunkSize options
        }

  liftIO $ newFastCDC readFunPtr chunkerOptsPtr
  where
    readSome :: Ptr Word8 -> CSize -> IO CInt
    readSome buf size = do
      putStrLn "readSome"
      bs <- popper (fromIntegral size)
      putStrLn "readSome done"
      let (fp, offset, len) = BS.Internal.toForeignPtr bs
      withForeignPtr fp $ \p -> do
        let p' = p `plusPtr` offset
        copyBytes buf p' len
        return $ fromIntegral len

withFastCDC ::
  forall t m.
  (MonadIO (t m), MonadResource (t m), MonadTrans t) =>
  FastCDCOptions ->
  (Int -> IO BS.ByteString) ->
  (Chunk -> t m ()) ->
  t m ()
withFastCDC options popper action = do
  (_, readFunPtr) <-
    allocate (FFI.c_wrap_reader_func readSome) freeHaskellFunPtr

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
      bs <- popper (fromIntegral size)
      let (fp, offset, len) = BS.Internal.toForeignPtr bs
      withForeignPtr fp $ \p -> do
        let p' = p `plusPtr` offset
        copyBytes buf p' len
        return $ fromIntegral len

    processChunks :: FastCDC -> t m ()
    processChunks chunker = go
      where
        go = do
          mchunk <- liftIO $ nextChunk chunker
          case mchunk of
            Just chunk | len chunk /= 0 -> do
              action chunk
              go
            _ -> return ()

nextChunk :: (MonadIO m) => FastCDC -> m (Maybe Chunk)
nextChunk (FastCDC chunkerFptr) = liftIO $
  withForeignPtr chunkerFptr $ \chunker -> do
    liftIO $ putStrLn "c_chunker_next"
    chunkPtr <- FFI.c_chunker_next chunker
    liftIO $ putStrLn "c_chunker_next done"

    if chunkPtr == nullPtr
      then return Nothing
      else do
        chunk <- peek chunkPtr
        let size = fromIntegral $ FFI.clength chunk

        -- Copy the chunk data into Haskell. Slow.
        -- bs <- BS.Internal.create size $ \p ->
        --   copyBytes p (FFI.cdata chunk) size

        -- Zero-copy ByteString.
        fptr <- newForeignPtr FFI.c_chunk_data_free (FFI.cdata chunk)
        let bs = BS.Internal.fromForeignPtr fptr 0 size

        -- Free the chunk, but not the data.
        FFI.c_chunk_metadata_free chunkPtr

        return $
          Just
            Chunk
              { hash = fromIntegral $ FFI.chash chunk,
                len = size,
                chunk = bs
              }
