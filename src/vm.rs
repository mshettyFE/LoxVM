#![allow(non_camel_case_types)]
use std::{cell::RefCell};
use std::rc::Rc;
use crate::{chunk::{Chunk, OpCode}, object::LoxString, compiler::isFalsey, scanner::Scanner, DEBUG_TRACE_EXEC};
use crate:: stack::LoxStack;
use crate::value::*;
use crate::compiler::*;
use crate::table::LoxTable;

use core::{fmt, panic};

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR(String),
    INTERPRET_RUNTIME_ERROR(String),
}

pub struct VM {
    chunk: Chunk, // currently executing chunk
    ip: usize, // index into code section of chunk denoting the next instruction to execute
    stk: LoxStack, // value stack 
    globals: LoxTable,                                    // global vars
}

impl VM {
    pub fn new() -> Self{
        return VM{chunk: Chunk::new(), ip: 0, 
        globals: LoxTable::new(),
        stk: LoxStack::new()};
    }
    
    pub fn interpret(&mut self, source: &String) -> InterpretResult { 
        let mut scanner = Scanner::new(source.to_string());
        let mut cnk = Chunk::new();
        let mut parser = Parser::new(&mut cnk);
        let mut comp = Compiler::new();
        // compile() returns false if an error occurred.
        if !parser.compile(&mut scanner, &mut comp, self){
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
                OpCode::OP_PRINT => {
                    match self.stk.pop(){
                        Some(v) => {
                            v.print_value();
                            println!();
                        }
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR("Stack is empty".to_string())}
                     }
                 }
                OpCode::OP_LOOP => {
                    let offset = self.read_short();
                    self.ip -= offset as usize;
                }
                OpCode::OP_JUMP =>{
                    let offset = self.read_short();
                    self.ip += offset as usize;
                }
                OpCode::OP_JUMP_IF_FALSE => {
                    let offset = self.read_short();
                    if isFalsey(self.stk.peek(0).unwrap()) {
                        self.ip += offset as usize;
                    }
                }
                OpCode::OP_RETURN => {
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
               OpCode::OP_POP => {
                    self.stk.pop();
               }
               OpCode::OP_GET_LOCAL => {
                    let slot = match self.read_byte() {
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
                    };
                    self.stk.push(self.stk.get(slot as usize).unwrap());
               }
               OpCode::OP_SET_LOCAL => {
                    let slot = match self.read_byte() {
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
                    };
                    self.stk.set(slot as usize, self.stk.peek(0).unwrap());
               }
               OpCode::OP_GET_GLOBAL => {
                    let constant_index = match self.read_byte(){
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
                    };
                    let name = match self.chunk.get_constant(usize::from(constant_index)){
                        Some(val) => val,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Couldn't access constant at address {}", constant_index))
                    }; 
                    let key: LoxString;
                    match name {
                        Value::VAL_OBJ(pointer_stuff) => {
                            let ptr = pointer_stuff.borrow();
                            key = ptr.any().downcast_ref::<LoxString>().unwrap().clone();
                        }
                        _ => {
                            return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Should have gotten a LoxString..."));
                        }
                    }
                    let wrapped_val = self.globals.find(key.clone());
                    match wrapped_val {
                        Some(value) => {
                            self.stk.push(value);
                        }
                        None => {
                            return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Undefined variable {}.",key.val.clone()));
                        }
                     }
               }
               OpCode::OP_DEFINE_GLOBAL => {
                    let constant_index = match self.read_byte(){
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
                    };
                    let name = match self.chunk.get_constant(usize::from(constant_index)){
                        Some(val) => val,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Couldn't access constant at address {}", constant_index))
                    };
                    match name {
                        Value::VAL_OBJ(pointer_stuff) => {
                               let ptr = pointer_stuff.borrow();
                               let key = ptr.any().downcast_ref::<LoxString>().unwrap();
                               let value = self.stk.peek(0).unwrap();
                               self.globals.insert(key.clone(), value);
                        },
                        _ => { return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Tried accessing a global"))}

                    } 
                    self.stk.pop();
               }
               OpCode::OP_SET_GLOBAL => {
                    let constant_index = match self.read_byte(){
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
                    };
                    let name = match self.chunk.get_constant(usize::from(constant_index)){
                        Some(val) => val,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Couldn't access constant at address {}", constant_index))
                    };
                    match name {
                        Value::VAL_OBJ(pointer_stuff) => {
                               let ptr = pointer_stuff.borrow();
                               let key = ptr.any().downcast_ref::<LoxString>().unwrap();
                               let value = self.stk.peek(0).unwrap();
                               if self.globals.insert(key.clone(), value).is_none(){
                                    return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Undefined variable {}.", key.val))
                               }
                        },
                        _ => { return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Tried accessing a global"))}
                    } 
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
                   self.stk.push(Value::VAL_BOOL(valuesEqual(a, b)));
               },
               OpCode::OP_GREATER => {
                   let a = match self.stk.peek(0){
                        Some(val) => {
                            match val {
                                Value::VAL_NUMBER(num) => {num},
                                _ => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Cant compare something greater than if it ain't a number"));}
                            }
                        },
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 0));},
                    };
                   let b = match self.stk.peek(1){
                        Some(val) => {
                            match val {
                                Value::VAL_NUMBER(num) => {num},
                                _ => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Cant compare something greater than if it ain't a number"));}
                            }
                        },
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 1));},
                    };
                   self.stk.push(Value::VAL_BOOL(b> a));
               },
               OpCode::OP_LESS => {
                   let a = match self.stk.peek(0){
                        Some(val) => {
                            match val {
                                Value::VAL_NUMBER(num) => {num},
                                _ => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Cant compare something less than if it ain't a number"));}
                            }
                        },
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 0));},
                    };
                   let b = match self.stk.peek(1){
                        Some(val) => {
                            match val {
                                Value::VAL_NUMBER(num) => {num},
                                _ => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Cant compare something less than if it ain't a number"));}
                            }
                        },
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Stack can't be accessed at {}", 1));},
                    };
                   self.stk.push(Value::VAL_BOOL(b< a));
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
               OpCode::OP_ADD => {
                   // check if top of stack are both strings
                   let a_string: Result<String, String> = match self.peek_stack(0, 
                       std::mem::discriminant(&Value::VAL_OBJ(Rc::new(RefCell::new(LoxString::new("".to_string())))))){
                        Ok(val) => { match val { Value::VAL_OBJ(v) => {
                            let a_obj = v.borrow(); 
                            Ok(a_obj.any().downcast_ref::<LoxString>().unwrap().val.clone())
                        }, _ => {panic!("Unreachable");}}}
                        Err(err_msg) => {Err(err_msg)},
                   };
                   let b_string: Result<String, String> = match self.peek_stack(1, 
                    std::mem::discriminant(&Value::VAL_OBJ(Rc::new(RefCell::new(LoxString::new("".to_string())))))){
                        Ok(val) => { match val { Value::VAL_OBJ(v) => {
                            let b_obj = v.borrow(); 
                            Ok(b_obj.any().downcast_ref::<LoxString>().unwrap().val.clone())
                        }, _ => {panic!("Unreachable");}}}
                        Err(err_msg) => {Err(err_msg)},
                   };

                    if (std::mem::discriminant(&a_string) == std::mem::discriminant(&b_string) )
                        && (std::mem::discriminant(&a_string) == std::mem::discriminant(&Ok("".to_string()))){
                            let a_val = a_string.unwrap();
                            let b_val  = b_string.unwrap();
                            let new_string = format!("{}{}",b_val, a_val);
                            self.stk.pop();
                            self.stk.pop();
                            self.stk.push(Value::VAL_OBJ(Rc::new(RefCell::new(LoxString::new(new_string)) )));
                        } 
                   else{
                        // doing floating point compariso
                       let a_float: Result<f64,String> = match self.peek_stack(0, std::mem::discriminant(&Value::VAL_NUMBER(0.0))){
                            Ok(val) => { match val { Value::VAL_NUMBER(v) => Ok(v), _ => {panic!("Unreachable");}}}
                            Err(err_msg) => {Err(err_msg)},
                       };
                       let b_float: Result<f64, String> = match self.peek_stack(1, std::mem::discriminant(&Value::VAL_NUMBER(0.0))){
                            Ok(val) => { match val { Value::VAL_NUMBER(v) => Ok(v), _ => {panic!("Unreachable");}}}
                            Err(err_msg) => {Err(err_msg)},
                        };
                        if std::mem::discriminant(&a_float) == std::mem::discriminant(&b_float) && 
                            (std::mem::discriminant(&a_float) == std::mem::discriminant(&Ok(0.0))){
                            // assuming you got here, you need to remove a and b from the stack first
                            self.stk.pop();
                            self.stk.pop();
    
                            self.stk.push(Value::VAL_NUMBER(a_float.unwrap()+b_float.unwrap()));
                             
                        } else {
                            return InterpretResult::INTERPRET_RUNTIME_ERROR("Operands must be two numbers or two strings.".to_string());
                        } 
                    }                     
               },
               OpCode::OP_SUBTRACT |
               OpCode::OP_DIVIDE |
               OpCode::OP_MULTIPLY => {
                        // doing floating point compariso
                       let a_float: Result<f64,String> = match self.peek_stack(0, std::mem::discriminant(&Value::VAL_NUMBER(0.0))){
                            Ok(val) => { match val { Value::VAL_NUMBER(v) => Ok(v), _ => {panic!("Unreachable");}}}
                            Err(err_msg) => {Err(err_msg)},
                       };
                       let b_float: Result<f64, String> = match self.peek_stack(1, std::mem::discriminant(&Value::VAL_NUMBER(0.0))){
                            Ok(val) => { match val { Value::VAL_NUMBER(v) => Ok(v), _ => {panic!("Unreachable");}}}
                            Err(err_msg) => {Err(err_msg)},
                        };
                        if std::mem::discriminant(&a_float) == std::mem::discriminant(&b_float) && 
                            (std::mem::discriminant(&a_float) == std::mem::discriminant(&Ok(0.0))){
                            // assuming you got here, you need to remove a and b from the stack first
                            self.stk.pop();
                            self.stk.pop();
    
                            let output = match opcode {
                                OpCode::OP_SUBTRACT => a_float.unwrap()-b_float.unwrap(),
                                OpCode::OP_MULTIPLY => a_float.unwrap()*b_float.unwrap(),
                                OpCode::OP_DIVIDE => a_float.unwrap()/b_float.unwrap(),
                                _ => return InterpretResult::INTERPRET_RUNTIME_ERROR("Something went horrible wrong when trying to do a binary operation".to_string())
                            };
                            self.stk.push(Value::VAL_NUMBER(output));     
                        }
                        else{ 
                            return InterpretResult::INTERPRET_RUNTIME_ERROR("Operands must be two numbers or two strings.".to_string()); 
                        }
               },
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
            None => {
                let err_msg = format!("Out of bounds access of code: {}", self.ip);
//                panic!("{}",err_msg);
                Err(err_msg)
            }
        };
        self.ip += 1;
        return output;
    }
 
    fn peek_stack(&mut self,  index :usize, expected: std::mem::Discriminant<Value>) -> Result<Value, String> {
        let b = match self.stk.peek(index){
            Some(v) => {
                // value matches expected value
                if std::mem::discriminant(&v) == expected{
                    v
                } else {
                    return Err(format!("stack at {} doesn't match expected: {:?}", index, &expected));
                }
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

    fn read_short(&mut self) -> u16{
        self.ip += 2;
       let higher_byte: u8 = *self.chunk.get_instr(self.ip-2).unwrap();
       let lower_byte: u8 = *self.chunk.get_instr(self.ip-1).unwrap();
       ((higher_byte as u16)  << (8 as u16)) | lower_byte as u16
    }
}
