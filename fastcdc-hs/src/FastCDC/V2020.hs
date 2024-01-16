module FastCDC.V2020
  ( FastCDCOptions(..)
  , Chunk(..)
  , withFastCDC
  ) where 

import qualified FastCDC.V2020.FFI as FFI

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS.Internal
import Foreign.C.Types
import Foreign.Ptr
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import UnliftIO.Foreign (withForeignPtr, alloca, copyBytes, Word8, poke)
import UnliftIO.Exception (bracket)



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


withFastCDC :: MonadUnliftIO m => FastCDCOptions -> BS.ByteString -> (Chunk -> m ()) -> m ()
withFastCDC options source action = do
    readFunPtr <- liftIO $ FFI.c_wrap_reader_func readSome

    alloca $ \chunkerOptsPtr -> do
      liftIO $ poke chunkerOptsPtr $ FFI.ChunkerOptions
        { FFI.minChunkSize = fromIntegral $ minChunkSize options
        , FFI.avgChunkSize = fromIntegral $ avgChunkSize options
        , FFI.maxChunkSize = fromIntegral $ maxChunkSize options
        }
      bracket (liftIO $ FFI.c_chunker_new readFunPtr chunkerOptsPtr) 
              (liftIO . FFI.c_chunker_free)
              processChunks
  where
    readSome :: MonadUnliftIO m => Ptr Word8 -> CSize -> m CInt
    readSome buf size = do
        let bs = BS.take (fromIntegral size) source
        let (fp, offset, len) = BS.Internal.toForeignPtr bs
        withForeignPtr fp $ \p -> do
            let p' = p `plusPtr` offset
            copyBytes buf p' len
            return $ fromIntegral len

    --processChunks :: MonadUnliftIO m => Ptr FFI.StreamCDC -> m ()
    processChunks chunker = do
      mchunk <- liftIO $ FFI.nextChunk chunker
      case mchunk of
        Just chunk -> unless (FFI.clength chunk == 0) $ do
            action $ Chunk
              { hash = mempty -- TODO: implement
              , len = fromIntegral $ FFI.clength chunk
              , chunk = mempty -- TODO: implement
              }
            processChunks chunker
        Nothing -> return ()