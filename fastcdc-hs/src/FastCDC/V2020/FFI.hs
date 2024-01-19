{-# LANGUAGE ForeignFunctionInterface #-}

module FastCDC.V2020.FFI
  ( -- * Types
    StreamCDC,
    ChunkerOptions (..),
    ChunkData (..),
    ReaderFunc,

    -- * FFI
    c_chunker_new,
    c_chunker_next,
    c_chunker_free,
    c_chunk_free,
    c_chunk_metadata_free,
    c_chunk_data_free,
    c_wrap_reader_func,
    c_get_last_error,
  )
where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data StreamCDC

data ChunkerOptions = ChunkerOptions
  { minChunkSize :: !CUInt,
    avgChunkSize :: !CUInt,
    maxChunkSize :: !CUInt
  }
  deriving stock (Show, Eq)

instance Storable ChunkerOptions where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = do
    minChunkSize <- peekByteOff ptr 0
    avgChunkSize <- peekByteOff ptr 4
    maxChunkSize <- peekByteOff ptr 8
    return $ ChunkerOptions {minChunkSize, avgChunkSize, maxChunkSize}
  poke ptr (ChunkerOptions {minChunkSize, avgChunkSize, maxChunkSize}) = do
    pokeByteOff ptr 0 minChunkSize
    pokeByteOff ptr 4 avgChunkSize
    pokeByteOff ptr 8 maxChunkSize

data ChunkData = ChunkData
  { -- | The gear value as of the end of the chunk.
    chash :: !CULLong,
    -- | Starting byte position within the source.
    coffset :: !CULLong,
    -- | Length of the chunk in bytes.
    clength :: !CSize,
    -- | Source bytes contained in this chunk.
    cdata :: !(Ptr Word8)
  }
  deriving stock (Show)

instance Storable ChunkData where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = do
    chash <- peekByteOff ptr 0
    coffset <- peekByteOff ptr 8
    clength <- peekByteOff ptr 16
    cdata <- peek (plusPtr ptr 24)
    return $ ChunkData {chash, coffset, clength, cdata}
  poke ptr (ChunkData {chash, coffset, clength, cdata}) = do
    pokeByteOff ptr 0 chash
    pokeByteOff ptr 8 coffset
    pokeByteOff ptr 16 clength
    poke (plusPtr ptr 24) cdata

type ReaderFunc =
  -- | A mutable buffer to write to.
  Ptr Word8 ->
  -- | The size of the write buffer.
  CSize ->
  -- | The number of bytes written or an error.
  IO CInt

foreign import ccall "wrapper" c_wrap_reader_func :: ReaderFunc -> IO (FunPtr ReaderFunc)

foreign import ccall "chunker_new" c_chunker_new :: FunPtr ReaderFunc -> Ptr ChunkerOptions -> IO (Ptr StreamCDC)

foreign import ccall safe "chunker_next" c_chunker_next :: Ptr StreamCDC -> IO (Ptr ChunkData)

foreign import ccall safe "&chunker_free" c_chunker_free :: FunPtr (Ptr StreamCDC -> IO ())

foreign import ccall safe "chunk_free" c_chunk_free :: Ptr ChunkData -> IO ()

foreign import ccall safe "chunk_metadata_free" c_chunk_metadata_free :: Ptr ChunkData -> IO ()

foreign import ccall safe "&chunk_data_free" c_chunk_data_free :: FunPtr (Ptr Word8 -> IO ())

foreign import ccall safe "get_last_error" c_get_last_error :: IO CString
