#![allow(non_snake_case)]
use LoxVM::vm::InterpretResult;
use LoxVM::vm::VM;

use std::fs;

use clap::{Parser, ArgAction};

use LoxVM::*;

// boilerplate to set up the command line parsing
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args{
    #[arg(default_value="None")]
    file_name: String,
    #[arg(short,long, action= ArgAction::SetFalse)]
    trace_flag: bool,
    #[arg(short,long, action= ArgAction::SetFalse)]
    print_code_flag: bool,
}

fn repl(){
    // really bare-bones REPL
    print!("Inside REPL\n");
    let mut machine = VM::new();
    let stdin = std::io::stdin();
    let mut buffer = String::new();
    loop{
           match stdin.read_line(&mut buffer) {
           Ok(_) => {
                match machine.interpret(&buffer){
                    InterpretResult::INTERPRET_OK => (),
                    InterpretResult::INTERPRET_COMPILE_ERROR(msg) => {println!("{}",msg); ()},
                    InterpretResult::INTERPRET_RUNTIME_ERROR(msg) => {println!("{}",msg); ()},
                }
           }
           Err(err_msg) => {
               eprintln!("{}", err_msg);
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
        InterpretResult::INTERPRET_COMPILE_ERROR(msg) => {eprintln!("{}",msg); std::process::exit(65)},
        InterpretResult::INTERPRET_RUNTIME_ERROR(msg) => {eprintln!("{}",msg); std::process::exit(65)},
    }
}

fn main() {
    let cli = Args::parse();
    DEBUG_TRACE_EXEC.set(cli.trace_flag).expect("Couldn't initialize debug trace flag");
    DEBUG_PRINT_CODE.set(cli.print_code_flag).expect("Couldn't initialize debug print code flag");
    if cli.file_name != "None" {
       runFile(cli.file_name.to_string());
    }
    repl();
}
