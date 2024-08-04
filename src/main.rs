use std::env;
use LoxVM::chunk::*;
use LoxVM::value::*;

fn main() {
    let _args: Vec<String> = env::args().collect();

    let mut chunk =  Chunk::new();
    let constant:usize = chunk.add_constant(Value::new(1.2));
    chunk.write_chunk(OpCode::OP_CONSTANT as u8, 123);
    chunk.write_chunk(constant as u8, 123);
    chunk.write_chunk(OpCode::OP_RETURN as u8, 123);

    _ = chunk.disassemble("test chunk".to_string());
}
