use hs_bindgen::*;
use fastcdc::v2020::*;
use std::io::Read;

#[hs_bindgen(new :: IO (Ptr Boop))]
pub extern "C" fn new() -> *mut StreamCDC<&mut [u8]> {
    let avg_chunk_size = 16384;
    let min_chunk_size = avg_chunk_size / 2;
    let max_chunk_size = avg_chunk_size * 4;
    let file = "hello world".as_ref();
    let chunker = StreamCDC::new(file, min_chunk_size, avg_chunk_size, max_chunk_size);
    Box::into_raw(Box::new(chunker))
}

// #[hs_bindgen("readChunk :: CUIntPtr AsyncStreamCDC -> ChunkData")]
// async fn read_chunk(chunker: &mut AsyncStreamCDC) -> ChunkData {
//     let chunk = chunker.read_chunk().await;
//     chunk.unwrap()
// }
