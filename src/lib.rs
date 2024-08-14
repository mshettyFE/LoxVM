#![allow(non_snake_case)]
pub mod chunk;
pub mod value;
pub mod vm;
pub mod stack;
pub mod scanner;
pub mod parser;

extern crate num;
#[macro_use]
extern crate num_derive;


use once_cell::sync::OnceCell;
pub static DEBUG_TRACE_EXEC: OnceCell<bool> = OnceCell::new();
pub static DEBUG_PRINT_CODE: OnceCell<bool> = OnceCell::new();

