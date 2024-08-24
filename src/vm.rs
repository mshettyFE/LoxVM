#![allow(non_camel_case_types)]
use crate::{chunk::{Chunk, OpCode}, parser::isFalsey, scanner::Scanner, DEBUG_TRACE_EXEC};
use crate:: stack::LoxStack;
use crate::value::*;
use crate::parser::Parser;

use core::fmt;

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR(String),
    INTERPRET_RUNTIME_ERROR(String),
}

pub struct VM {
    chunk: Chunk, // currently executing chunk
    ip: usize, // index into code section of chunk denoting the next instruction to execute
    stk: LoxStack, // value stack 
}

impl VM {
    pub fn new() -> Self{
        return VM{chunk: Chunk::new(), ip: 0, stk: LoxStack::new()};
    }
    
    pub fn interpret(&mut self, source: &String) -> InterpretResult { 
        let mut scanner = Scanner::new(source.to_string());
        let mut cnk = Chunk::new();
        let mut parser = Parser::new(&mut cnk);
        // compile() returns false if an error occurred.
        if !parser.compile(&mut scanner){
            return InterpretResult::INTERPRET_COMPILE_ERROR("Couldn't compile chunk".to_string());
        }
        self.chunk = cnk;
        self.ip = 0;
        let res = self.run();
        return res;
    }

    fn run(&mut self) -> InterpretResult {
        loop {
             match DEBUG_TRACE_EXEC.get() {
                 // assumes that each chunk has starting ip of 0
                 Some(val) => {
                     if *val {
                        self.stk.print();
                         _ = self.chunk.disassemble_instruction(self.ip);
                     }
                 }
                 None => panic!("DEBUG_TRACE_EXEC is somehow empty"),
                
             }
             let instruction_number  = match self.read_byte(){
                Ok(val) => val,
                Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
             };
           // try and convert current byte to an opcode
             let cand_opcode: Option<OpCode> = num::FromPrimitive::from_u8(instruction_number);
             let opcode = match cand_opcode {
                 Some(val) => val,
                None => return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Invalid conversion  to OpCode attempted: {}", instruction_number)),       
            };
            match opcode { //finally, dispatch to the correct opcode
                OpCode::OP_RETURN => {
                    match self.stk.pop(){
                        Some(v) => {
                            v.print_value();
                            println!();
                        }
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR("Stack is empty".to_string())}
                     }
                    return InterpretResult::INTERPRET_OK
                },
                OpCode::OP_CONSTANT => {
                    let constant_index = match self.read_byte(){
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
                    };
                    let constant = match self.chunk.get_constant(usize::from(constant_index)){
                        Some(val) => val,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Couldn't access constant at address {}", constant_index))
                    };
                    self.stk.push(constant.clone());
               },
               OpCode::OP_NIL => {
                    self.stk.push(Value::VAL_NIL)
               }
               OpCode::OP_TRUE => {
                    self.stk.push(Value::VAL_BOOL(true))
               }
               OpCode::OP_FALSE => {
                    self.stk.push(Value::VAL_BOOL(false))
               }
               OpCode::OP_EQUAL => {
                   let a = match self.stk.peek(0){
                        Some(val) => {val},
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 0));},
                    };
                   let b = match self.stk.peek(1){
                        Some(val) => {val},
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 1));},
                    };
                   self.stk.push(Value::VAL_BOOL(a==b));
               },
               OpCode::OP_GREATER => {
                   let a = match self.stk.peek(0){
                        Some(val) => {val},
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 0));},
                    };
                   let b = match self.stk.peek(1){
                        Some(val) => {val},
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 1));},
                    };
                   self.stk.push(Value::VAL_BOOL(a< b));
               },
               OpCode::OP_LESS => {
                   let a = match self.stk.peek(0){
                        Some(val) => {val},
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 0));},
                    };
                   let b = match self.stk.peek(1){
                        Some(val) => {val},
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 1));},
                    };
                   self.stk.push(Value::VAL_BOOL(a> b));
               },
               OpCode::OP_NEGATE => {
                    let peek_val = match self.stk.peek(0) {
                        Some(v) => v,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR("Stack is empty".to_string()),
                    };

                    match peek_val {
                        Value::VAL_NUMBER(num) => {
                            // remove from stack, but don't check value since we already have it
                            // from the peek operation
                            self.stk.pop();
                            self.stk.push(Value::VAL_NUMBER(-num) );
                        },
                        _ => {
                           return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Operand must be a number."))); 
                        }
                     }
               },
               OpCode::OP_ADD |
               OpCode::OP_SUBTRACT |
               OpCode::OP_DIVIDE |
               OpCode::OP_MULTIPLY => {
                   let a: f64 = match self.peek_num(0){
                        Ok(val) => {val},
                        Err(err_msg) => {return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg);},
                    };
                   let b: f64 = match self.peek_num(1){
                        Ok(val) => {val},
                        Err(err_msg) => {return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg);},
                   };
                    // assuming you got here, you need to remove a and b from the stack first
                    self.stk.pop();
                    self.stk.pop();

                    match opcode {
                        OpCode::OP_ADD => self.stk.push(Value::VAL_NUMBER(a+b)),
                        OpCode::OP_SUBTRACT => self.stk.push(Value::VAL_NUMBER(a-b)),
                        OpCode::OP_MULTIPLY => self.stk.push(Value::VAL_NUMBER(a*b)),
                        OpCode::OP_DIVIDE => self.stk.push(Value::VAL_NUMBER(a/b)),
                        _ => return InterpretResult::INTERPRET_RUNTIME_ERROR("Something went horrible wrong when trying to do a binary operation".to_string())
                    }
               }
               OpCode::OP_NOT => {
                   let val = self.stk.pop().unwrap();
                    self.stk.push(Value::VAL_BOOL(isFalsey(val)));
               }
            }
        }
    }

    fn read_byte(&mut self) -> Result<u8, String> {
        let output: Result<u8,String> = match self.chunk.get_instr(self.ip){
            Some(val) => Ok(*val),
            None => Err(format!("Out of bounds access of code: {}", self.ip)),
        };
        self.ip += 1;
        return output;
    }
 
    fn peek_num(&mut self,  index :usize) -> Result<f64, String> {
        let b = match self.stk.peek(index){
            Some(v) => {
                let out = match v{
                    Value::VAL_NUMBER(num) => num,
                    _ =>{return Err("Operands must be numbers.".to_string())}
                };
                out
            },
            None => return Err("Stack is empty".to_string())
        };
        Ok(b)
    }

    fn formatRunTimeError(&mut self, formatted_message: fmt::Arguments) -> String{
        let err_msg = format!("{}",formatted_message);
        // ip points to the NEXT instruction to be executed, so need to decrement ip by 1
        let instruction = self.ip-1;
        let line = self.chunk.get_line(instruction).unwrap();
        let line_err = format!("[line {}] in script\n", line);
        self.stk.reset();
        return format!("{}{}", err_msg,line_err);
    }
}
