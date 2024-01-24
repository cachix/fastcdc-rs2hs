#![feature(c_size_t)]
#![feature(slice_ptr_get)]

use core::cell::RefCell;
use core::ffi::c_size_t;
use fastcdc::v2020 as fastcdc;
use std::{ffi::*, io::Read, ptr};

pub struct Reader {
    inner: unsafe extern "C" fn(*mut u8, usize) -> c_int,
}

impl Read for Reader {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let len = unsafe { (self.inner)(buf.as_mut_ptr(), buf.len()) };
        match len {
            -1 => Err(std::io::Error::new(
                std::io::ErrorKind::Interrupted,
                "retry",
            )),
            l => {
                if l < -1 {
                    Err(std::io::Error::new(std::io::ErrorKind::Other, "read error"))
                } else {
                    Ok(l as usize)
                }
            }
        }
    }
}

// impl AsyncRead for Reader {
//     fn poll_read(
//         self: Pin<&mut Self>,
//         _cx: &mut Context<'_>,
//         buf: &mut tokio::io::ReadBuf<'_>,
//     ) -> Poll<Result<(), std::io::Error>> {
//         let me = self.get_mut();
//         let len = unsafe {
//             c_get_bytes(
//                 me.inner,
//                 buf.initialize_unfilled().as_mut_ptr(),
//                 buf.remaining(),
//             )
//         };
//         Poll::Ready(Ok(()))
//     }
// }

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
    data: *mut c_uchar,
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
    reader_ptr: extern "C" fn(*mut u8, usize) -> c_int,
    chunker_options: *const ChunkerOptions,
) -> *mut fastcdc::StreamCDC<Reader> {
    let ChunkerOptions {
        min_chunk_size,
        avg_chunk_size,
        max_chunk_size,
    } = unsafe { *chunker_options };
    let reader = Reader { inner: reader_ptr };
    let chunker = fastcdc::StreamCDC::new(reader, min_chunk_size, avg_chunk_size, max_chunk_size);
    Box::into_raw(Box::new(chunker))
}

// Store the last error string in a thread-local variable.
thread_local! {
    static LAST_ERROR: RefCell<Option<CString>> = const { RefCell::new(None) };
}

#[no_mangle]
pub unsafe extern "C" fn chunker_next(chunker: *mut fastcdc::StreamCDC<Reader>) -> *mut ChunkData {
    if chunker.is_null() {
        LAST_ERROR.with(|prev| {
            *prev.borrow_mut() = Some(CString::new("chunker is null").unwrap());
        });
        return ptr::null_mut();
    }

    let chunker = &mut *chunker;
    match chunker.next() {
        Some(Ok(chunk)) => {
            let cc = ChunkData::from_chunk(chunk);
            Box::into_raw(Box::new(cc))
        }
        Some(Err(err)) => {
            LAST_ERROR.with(|prev| {
                *prev.borrow_mut() = CString::new(err.to_string()).ok();
            });
            ptr::null_mut()
        }
        None => {
            LAST_ERROR.with(|prev| {
                *prev.borrow_mut() = Some(CString::new("no more chunks").unwrap());
            });
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn get_last_error() -> *const c_char {
    LAST_ERROR.with(|prev| match *prev.borrow() {
        Some(ref err) => err.as_ptr(),
        None => ptr::null(),
    })
}

#[no_mangle]
pub unsafe extern "C" fn chunker_free(chunker: *mut fastcdc::StreamCDC<Reader>) {
    let _ = Box::from_raw(chunker);
}

#[no_mangle]
pub unsafe extern "C" fn chunk_free(chunk: *mut ChunkData) {
    let chunk = Box::from_raw(chunk);
    let _ = Box::from_raw(chunk.data);
}

#[no_mangle]
pub unsafe extern "C" fn chunk_metadata_free(chunk: *mut ChunkData) {
    let _ = Box::from_raw(chunk);
}

#[no_mangle]
pub unsafe extern "C" fn chunk_data_free(chunk: *mut u8) {
    let _ = Box::from_raw(chunk);
}
