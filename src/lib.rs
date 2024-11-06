#![feature(variant_count)]
#![allow(non_snake_case)]
pub mod chunk;
pub mod value;
pub mod vm;
pub mod heap;
pub mod stack;
pub mod scanner;
pub mod compiler;
pub mod table;

extern crate num;
#[macro_use]
extern crate num_derive;

// define some global flags that get initialized at runtime via command line
use once_cell::sync::OnceCell;
pub static DEBUG_TRACE_EXEC: OnceCell<bool> = OnceCell::new();
pub static DEBUG_PRINT_CODE: OnceCell<bool> = OnceCell::new();
pub static DEBUG_STRESS_GC: OnceCell<bool> = OnceCell::new();
pub static DEBUG_LOG_GC: OnceCell<bool> = OnceCell::new();

