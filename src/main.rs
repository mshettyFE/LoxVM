use LoxVM::chunk::*;
use LoxVM::value::*;
use LoxVM::vm::InterpretResult;
use LoxVM::vm::VM;

use std::process;
use std::fs;

use clap::Parser;

use LoxVM::DEBUG_TRACE_EXEC;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args{
    #[arg(short,long)]
    debug_flag: bool,
    file_name: Option<String>
}

fn repl(){
    let mut machine = VM::new();
    let stdin = std::io::stdin();
    let mut buffer = String::new();
    loop{
        print!("> ");
        match stdin.read_line(&mut buffer) {
           Ok(_) => {
                machine.interpret(&buffer);
           }
           Err(err_msg) => {
               println!("{}", err_msg);
               break;
           }
        }
    } 
}

fn runFile(fname: String){
    let mut machine = VM::new();
    let source =  match fs::read_to_string(fname.clone()) {
        Ok(src) => src,
        Err(_) => {println!("Couldn't read in {}", fname); std::process::exit(65)},
    };
    match machine.interpret(&source){
        InterpretResult::INTERPRET_OK => std::process::exit(0),
        InterpretResult::INTERPRET_COMPILE_ERROR(msg) => {println!("{}",msg); std::process::exit(65)},
        InterpretResult::INTERPRET_RUNTIME_ERROR(msg) => {println!("{}",msg); std::process::exit(65)},
    }
}

fn main() {
    let cli = Args::parse();
    DEBUG_TRACE_EXEC.set(cli.debug_flag).expect("Couldn't initialize debug flag");
    match cli.file_name {
       Some(file_name) => runFile(file_name),
       None => repl(),
    }
}
