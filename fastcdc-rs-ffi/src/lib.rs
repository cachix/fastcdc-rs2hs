#![feature(c_size_t)]
#![feature(slice_ptr_get)]

use core::ffi::c_size_t;
use fastcdc::v2020 as fastcdc;
use std::fs::File;
use std::{ffi::*, ptr};

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct ChunkerOptions {
    min_chunk_size: c_uint,
    avg_chunk_size: c_uint,
    max_chunk_size: c_uint,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct ChunkData {
    hash: c_ulonglong,
    offset: c_ulonglong,
    length: c_size_t,
    data: *mut c_char,
}

impl ChunkData {
    fn new(hash: u64, offset: u64, length: usize, data: Vec<u8>) -> ChunkData {
        let cdata = Box::into_raw(data.into_boxed_slice()).as_mut_ptr();
        Self {
            hash,
            offset,
            length,
            data: cdata,
        }
    }

    fn from_chunk(chunk: fastcdc::ChunkData) -> ChunkData {
        Self::new(chunk.hash, chunk.offset, chunk.length, chunk.data)
    }
}

#[no_mangle]
pub unsafe extern "C" fn chunker_new(
    path: *const c_char,
    chunker_options: *const ChunkerOptions,
) -> *mut fastcdc::StreamCDC<File> {
    let file = unsafe {
        let path = CStr::from_ptr(path);
        std::fs::File::open(path.to_str().unwrap()).unwrap()
    };
    let ChunkerOptions {
        min_chunk_size,
        avg_chunk_size,
        max_chunk_size,
    } = unsafe { *chunker_options };
    let chunker = fastcdc::StreamCDC::new(file, min_chunk_size, avg_chunk_size, max_chunk_size);
    Box::into_raw(Box::new(chunker))
}

#[no_mangle]
pub unsafe extern "C" fn chunker_next(chunker: *mut fastcdc::StreamCDC<File>) -> *mut ChunkData {
    let chunker = unsafe { &mut *chunker };
    match chunker.next() {
        Some(Ok(chunk)) => {
            let cc = ChunkData::from_chunk(chunk);
            println!("rs chunk: {:?}", cc);
            Box::into_raw(Box::new(cc))
        }
        Some(Err(_err)) => ptr::null_mut(),
        None => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn chunker_free(chunker: *mut fastcdc::StreamCDC<File>) {
    let _ = Box::from_raw(chunker);
}
