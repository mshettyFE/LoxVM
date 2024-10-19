#![allow(non_camel_case_types)]

use crate::{chunk::OpCode, compiler::isFalsey, scanner::Scanner, DEBUG_TRACE_EXEC};
use crate:: stack::LoxStack;
use crate::value::*;
use crate::compiler::*;
use crate::table::LoxTable;

use core::{fmt, panic};
use std::time::{SystemTime, UNIX_EPOCH};
pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR(String),
    INTERPRET_RUNTIME_ERROR(String),
}

// the current function being executed, as well as where in the VM the function starts at
pub struct CallFrame{
    pub closure: Option<LoxClosure>,
    pub ip: usize, // ip relative to the current function (so 0 is the start of the current
                         // function, which has no relationship to other functions due to how the
                         // call stack is structured)
    pub starting_index: usize, // initial index into vm stack for this function
}

impl CallFrame {
    pub fn new(cls: Option<LoxClosure>, new_starting_index: usize) -> Self{
        CallFrame{closure: cls, ip: 0, starting_index: new_starting_index} 
    }
}

pub struct VM{
    frames: Vec<CallFrame>, // stores the function stack
    stk: LoxStack, // value stack 
    globals: LoxTable,  // global vars
    parser: Parser, // Bundled here b/c Rust is paranoid
}

pub fn clockNative(_argC: usize, _value_index: usize) -> Value{
    let time =     SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64();
    Value::VAL_NUMBER(time)
}

impl VM {
    pub fn new() -> Self{
    let mut x = VM{
        frames: Vec::new(),
        globals: LoxTable::new(),
        stk: LoxStack::new(),
        parser: Parser::new(),
        };

        // native functions get defined here
        x.defineNative(LoxString::new("clock".to_string()), clockNative);
        x
    }
    
    pub fn interpret(&mut self, source: &String) -> InterpretResult { 
        // entry point for VM
        let mut scanner = Scanner::new(source.to_string());
        self.parser = crate::compiler::Parser::new();
        // compile() returns false if a compilation error occurred.
        match self.parser.compile(&mut scanner){
            None => return InterpretResult::INTERPRET_COMPILE_ERROR("Couldn't compile chunk".to_string()),
            Some(comp) => {
                self.stk.push(Value::VAL_CLOSURE(LoxClosure { function: comp.function }));
                // this callValue simple executes the top level function
                match self.callValue(self.stk.peek(0).unwrap(), 0){
                        Err(str) => { return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",str)))}
                        Ok(()) => {()}
                }
                // run the top level function
                let res = self.run();
                return res;
            }
        }
    }

    fn run(&mut self) -> InterpretResult {
        loop {
             match DEBUG_TRACE_EXEC.get() {
                 // assumes that each chunk has starting ip of 0
                 Some(val) => {
                     if *val {
                        self.stk.print();
                        let frame = self.getCurrentFrame().unwrap();
                        match &frame.closure {
                            Some(cls) => {
                                cls.function.chunk.disassemble_instruction(frame.ip);
                           },
                            None => panic!("AAAAAAAA")
                       }
                    }
                 }
                 None => panic!("DEBUG_TRACE_EXEC is somehow empty"), 
             }
             // get current instruction to run in the current chunk
             let instruction_number  = match self.read_byte(){
                Ok(val) => val,
                Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",err_msg))),
             };
           // try and convert current byte to an opcode
             let cand_opcode: Option<OpCode> = num::FromPrimitive::from_u8(instruction_number);
             let opcode = match cand_opcode {
                 Some(val) => val,
                None => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Invalid conversion  to OpCode attempted: {}", instruction_number))),       
            };
            match opcode { //finally, dispatch to the correct opcode
                OpCode::OP_PRINT => {
                    match self.stk.pop(){
                        Some(v) => {
                            v.print_value();
                            println!();
                        }
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Stack is empty")))}
                     }
                 }
                OpCode::OP_LOOP => {
                    let offset = self.read_short(); // How many addresses back do we need to jump
                                                    // backwards
                    self.getCurrentFrame().unwrap().ip -= offset as usize;
                }
                OpCode::OP_JUMP =>{ // unconditional jump
                    let offset = self.read_short(); // How many addresses do we need to jump ahead
                    self.getCurrentFrame().unwrap().ip += offset as usize;
                }
                OpCode::OP_JUMP_IF_FALSE => {
                    let offset = self.read_short();
                    if isFalsey(self.stk.peek(0).unwrap()) { // If top of stack if false, jump
                                                             // ahead
                        self.getCurrentFrame().unwrap().ip += offset as usize;
                    }
                },
                OpCode::OP_CALL => { // execute the function
                    let argCount = self.read_byte().unwrap();
                    match (self.callValue(self.stk.peek(argCount as usize).unwrap(), argCount)){
                        Err(str) => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",str)))}
                        Ok(()) => {()}
                    }
                },
                OpCode::OP_CLOSURE => {
                    if let Value::VAL_FUNCTION(func) = self.read_constant(){
                        self.stk.push(Value::VAL_CLOSURE(LoxClosure{function: func}))
                    } else {panic!()}
              },
               OpCode::OP_GET_UPVALUE => {
                   todo!()
              },
               OpCode::OP_SET_UPVALUE => {
                   todo!()
              },
                OpCode::OP_RETURN => { // return from function
                    let result = self.stk.pop();
                    // clean up the stack to until the current function call gets erased
                    let current_pointer = self.getCurrentFrame().unwrap().starting_index;
                    while self.stk.size() > current_pointer{
                        self.stk.pop();
                    }
                    // Pop from the call stack
                    self.frames.pop();
                    // If at the base, then you need to exit from the program
                    if (self.frames.len() == 0){
                        self.stk.pop(); // remove the stray NIL value from the stack
                        return InterpretResult::INTERPRET_OK
                    }
                    // add the return value to the VM value stack
                    self.stk.push(result.unwrap());
                },
                OpCode::OP_CONSTANT => { // add constant to stack
                  let constant: Value = self.read_constant();
                 self.stk.push(constant.clone());
               },
               OpCode::OP_NIL => {
                    self.stk.push(Value::VAL_NIL);
               }
               OpCode::OP_TRUE => {
                    self.stk.push(Value::VAL_BOOL(true))
               }
               OpCode::OP_FALSE => {
                    self.stk.push(Value::VAL_BOOL(false))
               }
               OpCode::OP_POP => {
                    self.stk.pop();
               }
               OpCode::OP_GET_LOCAL => { // get local variable, and push to stack
                    let slot = match self.read_byte() {
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",err_msg))),
                    };
                    let cur_frame = self.getCurrentFrame().unwrap();
                    let index = cur_frame.starting_index + slot as usize +1;
                    self.stk.push(self.stk.get(index).unwrap());
               }
               OpCode::OP_SET_LOCAL => { // get location of variable in stack, then update value
                    let slot = match self.read_byte() {
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",err_msg))),
                    };
                    let cur_frame = self.getCurrentFrame().unwrap();
                    let index = cur_frame.starting_index + slot as usize+1;
//                    println!("ABS Index {}, starting {}, slot {}", index, cur_frame.starting_index, slot);
                    self.stk.set(index, self.stk.peek(0).unwrap());
               }
               OpCode::OP_GET_GLOBAL => { // get value of global variable from the global hash
                                          // table
                    let name: Value =  self.read_constant();
                    let key: LoxString;
                    match name {
                        Value::VAL_STRING(new_key) => {
                            key = new_key;
                        }
                        _ => {
                            return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Should have gotten a LoxString...")));
                        }
                    }
                    let wrapped_val = self.globals.find(key.clone());
                    match wrapped_val {
                        Some(value) => {
                            self.stk.push(value);
                        }
                        None => {
                            return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Undefined variable {}.",key.name.clone())));
                        }
                     }
               }
               OpCode::OP_DEFINE_GLOBAL => { // Creating a new global variable 
                    let name: Value = self.read_constant();
                    match name {
                        Value::VAL_STRING(str) => {
                               let value = self.stk.peek(0).unwrap();
                               self.globals.insert(str, value);
                        },
                        _ => { return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Tried accessing a global")))}

                    } 
                    self.stk.pop();
               }
               OpCode::OP_SET_GLOBAL => { // updating an existing variable
                    let name: Value = self.read_constant();
                    match name {
                        Value::VAL_STRING(key) => {
                               let value = self.stk.peek(0).unwrap();
                               if self.globals.insert(key.clone(), value).is_none(){
                                    return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Undefined variable {}.", key.name)))
                               }
                        },
                        _ => { return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}","Tried accessing a global")));}
                    } 
               }
               OpCode::OP_EQUAL => { // check if top two values of stack are equal
                   let a = match self.stk.peek(0){
                        Some(val) => {val},
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{} {}","Stack can't be accessed at", 0)));},
                    };
                   let b = match self.stk.peek(1){
                        Some(val) => {val},
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{} {}","Stack can't be accessed at", 1)));},
                    };
                   self.stk.push(Value::VAL_BOOL(valuesEqual(a, b)));
               },
               OpCode::OP_GREATER => { // same as OP_EQUAL, but greater
                   let a = match self.stk.peek(0){
                        Some(val) => {
                            match val {
                                Value::VAL_NUMBER(num) => {num},
                                _ => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}","Cant compare something greater than if it ain't a number")));}
                            }
                        },
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}{}","Stack can't be accessed at ",0)));},
                    };
                   let b = match self.stk.peek(1){
                        Some(val) => {
                            match val {
                                Value::VAL_NUMBER(num) => {num},
                                _ => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}","Cant compare something greater than if it ain't a number")));}
                            }
                        },
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}{}","Stack can't be accessed at ", 1)));},
                    };
                   self.stk.push(Value::VAL_BOOL(b> a));
               },
               OpCode::OP_LESS => { // same as OP_EQUAL, but less than
                   let a = match self.stk.peek(0){
                        Some(val) => {
                            match val {
                                Value::VAL_NUMBER(num) => {num},
                                _ => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}","Cant compare something less than if it ain't a number")));}
                            }
                        },
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{} {}","Stack can't be accessed at ",0)));},
                    };
                   let b = match self.stk.peek(1){
                        Some(val) => {
                            match val {
                                Value::VAL_NUMBER(num) => {num},
                                _ => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}","Cant compare something less than if it ain't a number")));}
                            }
                        },
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}{}","Stack can't be accessed at {}", 1)));},
                    };
                   self.stk.push(Value::VAL_BOOL(b< a));
               },
               OpCode::OP_NEGATE => { // replace top value in stack with the negation
                    let peek_val = match self.stk.peek(0) {
                        Some(v) => v,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Stack is empty"))),
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
               OpCode::OP_ADD => { // addition of strings and numbers
                   // check if top of stack are both strings
                   let a = self.stk.peek(0).expect("Unreachable");
                   let b = self.stk.peek(1).expect("Unreachable");
                   if a.get_type() == b.get_type() {
                        let new_val = match a.get_type(){
                            LoxType::STRING => {
                                if let Value::VAL_STRING(a_val) = a{
                                    if let Value::VAL_STRING(b_val) = b{
                                        let new_str = format!("{}{}", b_val.name, a_val.name);
                                        Value::VAL_STRING(LoxString::new(new_str))
                                    } else {panic!()}
                                } else {panic!()}
                            },
                             LoxType::NUMBER => {
                                if let Value::VAL_NUMBER(a_val) = a{
                                    if let Value::VAL_NUMBER(b_val) = b{
                                        Value::VAL_NUMBER(a_val+b_val)
                                    } else {panic!()}
                                } else {panic!()}
                            },
                            _ => {
                                return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Operands must be two numbers or two strings.")));
                            }
                        };
                        self.stk.pop();
                        self.stk.pop();
                        self.stk.push(new_val);
                   }
               },
               OpCode::OP_SUBTRACT |
               OpCode::OP_DIVIDE |
               OpCode::OP_MULTIPLY => {
                   let a = self.stk.peek(0).expect("Unreachable");
                   let b = self.stk.peek(1).expect("Unreachable");
                   if a.get_type() == b.get_type() && (a.get_type() == LoxType::NUMBER) {
                       self.stk.pop();
                       self.stk.pop();
                       if let Value::VAL_NUMBER(a_float) = a {
                            if let Value::VAL_NUMBER(b_float) = b{
                               let output = match opcode {
                                    OpCode::OP_SUBTRACT => { b_float-a_float},
                                    OpCode::OP_MULTIPLY => b_float*a_float,
                                    OpCode::OP_DIVIDE => b_float/a_float,
                                    _ => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Something went horrible wrong when trying to do a binary operation")))
                                };
                                self.stk.push(Value::VAL_NUMBER(output));          
                            } else {panic!()}
                       } else {panic!()}
                   } else {
                        return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Operands must be two numbers or two strings."))); 
                   }
              },
               OpCode::OP_NOT => { // add the logical negation of top of stack
                   let val = self.stk.pop().unwrap();
                    self.stk.push(Value::VAL_BOOL(isFalsey(val)));
               }
            }
        }
    }

    fn callValue(&mut self, callee: Value, argCount: u8)-> Result<(),String>{
        // This is a wrapper around Call. It does type checking on the input Value to make sure
        // it's a callable
        match callee {
            Value::VAL_CLOSURE(cls) => {
                return self.Call( cls, argCount);
            },
           Value::VAL_NATIVE(native_func) => {
                        let fn_start = self.stk.size()- argCount as usize;
                        // execute the native function and  push the result onto the VM stack
                        let result = native_func(argCount as usize, fn_start);
                        while (self.stk.size() > fn_start){
                            self.stk.pop();
                        }
                        self.stk.push(result);
                        return Ok(());
                }
            _ => {
                return Err("Can only call functions and classes.".to_string());
            }
        }
    }

    fn Call(&mut self, closure: LoxClosure , argCount: u8) -> Result<(),String>{
        // This does runtime checking that the number of input arguments matches the arity of the
        // function
        if(self.frames.len()+1 == 255){
            return Err("Stack overflow.".to_string());
        }
            if argCount as usize != closure.function.arity {
                    return Err(format!("Expected {} arguments but got {}.", closure.function.arity, argCount));
        }
        // Adds a new frame to the call stack
            self.frames.push( CallFrame::new(Some(closure), self.stk.size() - argCount as usize -1 )); 
       return Ok(());
    } 

    fn read_byte(&mut self) -> Result<u8, String> {
        // read one instruction from the current chunk
        // also advance ip by 1
        let frame = self.getCurrentFrame();
        let f = frame.unwrap();

        let output = match &f.closure{
            Some(cls) =>{
               match cls.function.chunk.get_instr(f.ip){
                Some(val) => Ok(*val),
                None => {
                  let err_msg = format!("Out of bounds access of code: {}", f.ip);
                  Err(err_msg)
                }
              }
            }
            None => panic!()
        };
        f.ip += 1;
        return output;
    }
 
    fn formatRunTimeError(&mut self, formatted_message: fmt::Arguments) -> String{
        // pretty printing runtime errors
        let err_msg = format!("{}\n",formatted_message);
        let frame = self.getCurrentFrame().unwrap();
        // ip points to the NEXT instruction to be executed, so need to decrement ip by 1
        let instruction = frame.ip - 1;
        println!("Current Instr: {}", instruction);
        let line: usize;
        let fname: String;
        match &frame.closure{
            Some(cls) => {
                fname = cls.function.name.name.clone();
                match cls.function.chunk.get_line(instruction){
                    Some(val) => line = val.clone(),
                    None => panic!()
                }
            }
            None => panic!()
        }
      let line_err = format!("[line {}] in {}\n", line, fname);

        // stack trace
        let mut cur_frame = self.frames.len()-1;
        let mut stack_trace = "".to_string();
        while cur_frame >= 0 {
            match &self.frames.get_mut(cur_frame){
                None => {
                    panic!("AAAAAAAAAAAAAAA");
                }
                Some(frame) =>{
                    match &frame.closure{
                        Some(cls) => {
                            stack_trace = format!("{} [line {}] in {}\n", stack_trace, cls.function.chunk.get_line(frame.ip).unwrap(), cls.function.name.name);
                       },
                        None => panic!("AAAAAAAAAA")
                    }
                }
            }
            if cur_frame == 0 {break;}
            cur_frame -= 1;
        }

        self.stk.reset();
        return format!("{}{}{}", line_err, err_msg, stack_trace);
    }

    fn defineNative(&mut self, name: LoxString, function: NativeFn){
        // The pushes and pops are weird garbage collector things that aren't really necessary in
        // a Rust-based VM
        self.stk.push(Value::VAL_STRING(name.clone()));
        self.stk.push(Value::VAL_NATIVE(function));
        self.globals.insert( name, self.stk.get(1).unwrap());
        self.stk.pop();
        self.stk.pop();
    }

    fn read_short(&mut self) -> u16{
        // read the next two bytes in the chunk, incrementing the ip as needed
        self.getCurrentFrame().unwrap().ip += 2;
        let ip = self.getCurrentFrame().unwrap().ip;
        let higher_byte: u16;
        let lower_byte: u16;
        match &self.getCurrentFrame().unwrap().closure {
            Some(cls) => {
                higher_byte = *cls.function.chunk.get_instr(ip-2).unwrap() as u16;
                lower_byte = *cls.function.chunk.get_instr(ip-1).unwrap() as u16;
           }
            None => panic!("AAAAAAAAA")
       }
//        println!("High {}, Low {}, IP {}", higher_byte, lower_byte, ip );
        return  (higher_byte << 8 ) | lower_byte;
    }

    fn read_constant(&mut self) -> Value {
        // read a value from the chunk and return the value. Don't mess with the stack
        let index = self.read_byte().unwrap();
        let frame = self.getCurrentFrame().unwrap();
        let output: Value;
        match &frame.closure{
            Some(cls) => {
               output = cls.function.chunk.get_constant(index as usize)
                   .expect(&format!("Out of bounds access of code: {}", frame.ip).to_owned()).clone();
            }
            None => {
                panic!("Invalid function");
            }
        }
        return output;
    }

    fn getCurrentFrame(&mut self) -> Option<&mut CallFrame>{
        // helper function to get the top CallFrame
        let length = self.frames.len();
        self.frames.get_mut(length-1)
    }
}
