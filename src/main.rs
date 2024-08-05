use LoxVM::chunk::*;
use LoxVM::value::*;
use LoxVM::vm::VM;

use clap::Parser;

use LoxVM::DEBUG_TRACE_EXEC;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args{
    #[arg(short,long)]
    debug_flag: bool,
}

fn main() {
    let cli = Args::parse();
    DEBUG_TRACE_EXEC.set(cli.debug_flag).expect("Couldn't initialize debug flag");

    let mut machine = VM::new();

    let mut chunk =  Chunk::new();

    let mut constant = chunk.add_constant(Value::new(1.2));
	chunk.write_chunk(OpCode::OP_CONSTANT as u8, 123);
	chunk.write_chunk(constant as u8, 123);
    constant = chunk.add_constant(Value::new(3.4));
	chunk.write_chunk(OpCode::OP_CONSTANT as u8, 123);
	chunk.write_chunk(constant as u8, 123);
	chunk.write_chunk(OpCode::OP_ADD as u8 , 123);
    constant = chunk.add_constant(Value::new(5.6));
	chunk.write_chunk(OpCode::OP_CONSTANT as u8, 123);
	chunk.write_chunk(constant as u8 , 123);
	chunk.write_chunk(OpCode::OP_DIVIDE as u8, 123);
	chunk.write_chunk(OpCode::OP_NEGATE as u8, 123);
	chunk.write_chunk(OpCode::OP_RETURN as u8, 123);

    let constant:usize = chunk.add_constant(Value::new(1.2));
    chunk.write_chunk(OpCode::OP_CONSTANT as u8, 123);
    chunk.write_chunk(constant as u8, 123);
    chunk.write_chunk(OpCode::OP_NEGATE as u8, 123);
    chunk.write_chunk(OpCode::OP_RETURN as u8, 123);

    machine.interpret(chunk);
}
