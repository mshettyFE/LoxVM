#![allow(non_camel_case_types)]
use std::{cell::RefCell};
use std::rc::Rc;

use crate::object::{LoxFunction, NativeFn, Obj, ObjNative};
use crate::{chunk::{Chunk, OpCode}, object::LoxString, compiler::isFalsey, scanner::Scanner, DEBUG_TRACE_EXEC};
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

pub struct CallFrame{
    pub func: Option<Rc<RefCell<LoxFunction>>>,
    pub ip: usize, // ip relative to the current function (so 0 is the start of the current
                         // function, which has no relationship to other functions due to how the
                         // call stack is structured)
    pub starting_index: usize, // initial index into vm stack for this function
}

impl CallFrame {
    pub fn new(function: Rc<RefCell<LoxFunction>>, new_starting_index: usize) -> Self{
        CallFrame{func: Some(function), ip: 0, starting_index: new_starting_index} 
    }
}

pub struct VM{
    frames: Vec<CallFrame>,
    frameCount: usize,
    stk: LoxStack, // value stack 
    globals: LoxTable,                                    // global vars
    parser: Parser
}

pub fn clockNative(argC: usize, value_index: usize) -> Value{
    let time =     SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64();
    Value::VAL_NUMBER(time)
}

impl VM {
    pub fn new() -> Self{
    let mut x = VM{
        frames: Vec::new(),
        frameCount: 0,
        globals: LoxTable::new(),
        stk: LoxStack::new(),
        parser: Parser::new()
        };
    
        x.defineNative(LoxString::new("clock".to_string()), clockNative);
        x
    }
    
    pub fn interpret(&mut self, source: &String) -> InterpretResult { 
        let mut scanner = Scanner::new(source.to_string());
        self.parser = crate::compiler::Parser::new();
        // compile() returns false if an error occurred.
        match self.parser.compile(&mut scanner){
            None => return InterpretResult::INTERPRET_COMPILE_ERROR("Couldn't compile chunk".to_string()),
            Some(comp) => {
                self.stk.push(Value::VAL_OBJ(Rc::new(RefCell::new(comp.function.unwrap())).clone()));
                match self.callValue(self.stk.peek(0).unwrap(), 0){
                        Err(str) => { return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",str)))}
                        Ok(()) => {()}
                }
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
                        let frame = self.getCurrentFunction().unwrap();
                        match &frame.func {
                            Some(fun) => {
                                let _ = fun.borrow().chunk.disassemble_instruction(frame.ip);
                            }
                            None => {
                                panic!("AAAAAAAAA");
                            }
                        }
                    }
                 }
                 None => panic!("DEBUG_TRACE_EXEC is somehow empty"),
                
             }
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
//            println!("Cur Frame {}, MaxFrames {}", self.frameCount, self.frames.len());
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
                    let offset = self.read_short();
                    self.getCurrentFunction().unwrap().ip -= offset as usize;
                }
                OpCode::OP_JUMP =>{
                    let offset = self.read_short();
                    self.getCurrentFunction().unwrap().ip += offset as usize;
                }
                OpCode::OP_JUMP_IF_FALSE => {
                    let offset = self.read_short();
                    if isFalsey(self.stk.peek(0).unwrap()) {
                        self.getCurrentFunction().unwrap().ip += offset as usize;
                    }
                },
                OpCode::OP_CALL => {
                    //println!("Function  Depth {}", self.frameCount);
                    let argCount = self.read_byte().unwrap();
                    match (self.callValue(self.stk.peek(argCount as usize).unwrap(), argCount)){
                        Err(str) => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",str)))}
                        Ok(()) => {()}
                    }
                }
                OpCode::OP_RETURN => {
                    let result = self.stk.pop();
                    let current_pointer = self.getCurrentFunction().unwrap().starting_index;
/*
                    for frame in self.frames.iter(){
                        println!("FuncStart {}", frame.starting_index);
                    }
                    println!("Returning from. FrameCount {}, TotalFrames {}, Stack Size {}, cur_ptr {}", self.frameCount , self.frames.len(),  self.stk.size(), current_pointer);
*/
                    while self.stk.size() > current_pointer{
                        self.stk.pop();
                    }
                    self.frameCount -= 1;
                    self.frames.pop();
                    if (self.frameCount == 0){
                        self.stk.pop();
                        return InterpretResult::INTERPRET_OK
                    }
                    self.stk.push(result.unwrap());
                },
                OpCode::OP_CONSTANT => {
                  let constant: Value = self.read_constant();
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
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",err_msg))),
                    };
                    let cur_frame = self.getCurrentFunction().unwrap();
                    let index = cur_frame.starting_index + slot as usize +1;
//                    println!("ABS Index {}, starting {}, slot {}", index, cur_frame.starting_index, slot);
                    self.stk.push(self.stk.get(index).unwrap());
               }
               OpCode::OP_SET_LOCAL => {
                    let slot = match self.read_byte() {
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",err_msg))),
                    };
                    let cur_frame = self.getCurrentFunction().unwrap();
                    let index = cur_frame.starting_index + slot as usize+1;
//                    println!("ABS Index {}, starting {}, slot {}", index, cur_frame.starting_index, slot);
                    self.stk.set(index, self.stk.peek(0).unwrap());
               }
               OpCode::OP_GET_GLOBAL => {
                    let name: Value =  self.read_constant();
                    let key: LoxString;
                    match name {
                        Value::VAL_OBJ(pointer_stuff) => {
                            let ptr = pointer_stuff.borrow();
                            key = ptr.any().downcast_ref::<LoxString>().unwrap().clone();
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
                            return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Undefined variable {}.",key.val.clone())));
                        }
                     }
               }
               OpCode::OP_DEFINE_GLOBAL => {
                    let name: Value = self.read_constant();
                    match name {
                        Value::VAL_OBJ(pointer_stuff) => {
                               let ptr = pointer_stuff.borrow();
                               let key = ptr.any().downcast_ref::<LoxString>().unwrap();
                               let value = self.stk.peek(0).unwrap();
                               self.globals.insert(key.clone(), value);
                        },
                        _ => { return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Tried accessing a global")))}

                    } 
                    self.stk.pop();
               }
               OpCode::OP_SET_GLOBAL => {
                    let name: Value = self.read_constant();
                    match name {
                        Value::VAL_OBJ(pointer_stuff) => {
                               let ptr = pointer_stuff.borrow();
                               let key = ptr.any().downcast_ref::<LoxString>().unwrap();
                               let value = self.stk.peek(0).unwrap();
                               if self.globals.insert(key.clone(), value).is_none(){
                                    return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Undefined variable {}.", key.val)))
                               }
                        },
                        _ => { return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}","Tried accessing a global")));}
                    } 
               }
               OpCode::OP_EQUAL => {
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
               OpCode::OP_GREATER => {
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
               OpCode::OP_LESS => {
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
               OpCode::OP_NEGATE => {
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
                            return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Operands must be two numbers or two strings.")));
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
                                OpCode::OP_SUBTRACT => b_float.unwrap()-a_float.unwrap(),
                                OpCode::OP_MULTIPLY => b_float.unwrap()*a_float.unwrap(),
                                OpCode::OP_DIVIDE => b_float.unwrap()/a_float.unwrap(),
                                _ => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Something went horrible wrong when trying to do a binary operation")))
                            };
                            self.stk.push(Value::VAL_NUMBER(output));     
                        }
                        else{ 
                            return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Operands must be two numbers or two strings."))); 
                        }
               },
               OpCode::OP_NOT => {
                   let val = self.stk.pop().unwrap();
                    self.stk.push(Value::VAL_BOOL(isFalsey(val)));
               }
            }
        }
    }

    fn callValue(&mut self, callee: Value, argCount: u8)-> Result<(),String>{
        match callee {
            Value::VAL_OBJ(obj_ptr) => {
                let obj = obj_ptr.borrow();
                match obj.get_type() {
                    crate::object::ObjType::OBJ_FUNCTION => {
                        return self.Call(obj_ptr.clone(), argCount);
                    },
                    crate::object::ObjType::OBJ_NATIVE => {
                        // the value argument is wrong, but IDK
                        let fn_start = self.stk.size()- argCount as usize;
                        let result = (obj.any().downcast_ref::<ObjNative>().unwrap().function)(argCount as usize, fn_start);
                        while (self.stk.size() > fn_start){
                            self.stk.pop();
                        }
                        self.stk.push(result);
                        return Ok(());
                    }
                    _ => {()} 
                }
            }
            _ => {()}
        }
        return Err("Can only call functions and classes.".to_string());
    }

    fn Call(&mut self, function_wrapper: Rc<RefCell<dyn Obj>> , argCount: u8) -> Result<(),String>{
        self.frameCount += 1;
        if(self.frameCount == 255){
            return Err("Stack overflow.".to_string());
        }
        let function = function_wrapper.borrow();
        if argCount as usize != function.any().downcast_ref::<LoxFunction>().unwrap().arity {
            return Err(format!("Expected {} arguments but got {}.",function.any().downcast_ref::<LoxFunction>().unwrap().arity, argCount));
        }
        self.frames.push(
            CallFrame::new(Rc::new(RefCell::new(function.any().downcast_ref::<LoxFunction>().unwrap().clone())),
            self.stk.size() - argCount as usize -1 )
        );
        return Ok(());
    } 

    fn read_byte(&mut self) -> Result<u8, String> {
        let frame = self.getCurrentFunction();
        let f = frame.unwrap();
        let output: Result<u8,String>;
        match &f.func {
            Some(fnc) => {
                match fnc.borrow().chunk.get_instr(f.ip) {
                    Some(val) => output = Ok(*val),
                    None => {
                    let err_msg = format!("Out of bounds access of code: {}", f.ip);
//                    panic!("{}", err_msg);
                    return Err(err_msg);
                    }
                }
                f.ip += 1;
            },
            None => panic!("AAAAAAAAAAAA"),
        }
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
        let err_msg = format!("{}\n",formatted_message);
        let frame = self.getCurrentFunction().unwrap();
        let instruction = frame.ip - 1;
        // ip points to the NEXT instruction to be executed, so need to decrement ip by 1
        let line: usize;
        match &frame.func{
            Some(fnc) => {
               match fnc.borrow().chunk.get_line(instruction) {
                    None => {
                        panic!("AAAAA");
                    }
                    Some(val) => {line =  val.clone();} 
               }
            },
            None => {
                panic!("Invalid function");
            }
        }
        let fname =  match &frame.func{
            Some(function) => {
                match &function.borrow().name{
                    Some(name) => name.val.clone(),
                    None => "script".to_string(),
                }
            },
            None => "script".to_string()
        };
        let line_err = format!("[line {}] in {}\n", line, fname);

        // stack trace
        let mut cur_frame = self.frameCount-1;
        let mut stack_trace = "".to_string();
        while cur_frame >= 0 {
            match &self.frames.get_mut(cur_frame){
                None => {
                    panic!("AAAAAAAAAAAAAAA");
                }
                Some(frame) =>{
                    match &frame.func{
                        None => panic!("AAAAAAAAAA"),
                        Some(function) => {
                            let f = function.borrow();                    
                            let instr = frame.ip;
                            let line_num = f.chunk.get_line(instr);
                            stack_trace = format!("{}[line {}] in", stack_trace, line_num.unwrap());
                            match &f.name{
                                None => {
                                    stack_trace = format!("{} script\n",stack_trace );
                                }
                                Some(string) => {
                                    stack_trace = format!("{} {}()\n",stack_trace, string.val.clone() );
                                }
                            }
                        }
                    }
                }
            }
            if (cur_frame == 0) {break;}
            cur_frame -= 1;
        }

        self.stk.reset();
        return format!("{}{}{}", line_err, err_msg, stack_trace);
    }

    fn defineNative(&mut self, name: LoxString, function: NativeFn){
        self.stk.push(Value::VAL_OBJ(Rc::new( RefCell::new(name.clone()))));
        self.stk.push(Value::VAL_OBJ(Rc::new( RefCell::new(ObjNative::new(function)))));
        self.globals.insert( name, self.stk.get(1).unwrap());
        self.stk.pop();
        self.stk.pop();
    }

    fn read_short(&mut self) -> u16{
        self.getCurrentFunction().unwrap().ip += 2;
        let ip = self.getCurrentFunction().unwrap().ip;
        let higher_byte: u16;
        let lower_byte: u16;
        match &self.getCurrentFunction().unwrap().func {
            Some(fnc) => {
                match fnc.borrow().chunk.get_instr(ip-2) {
                    Some(val) => higher_byte = *val as u16,
                    None => {
                        panic!("Out of bounds access of code: {}", ip);
                    }
                }
                match fnc.borrow().chunk.get_instr(ip-1){
                    Some(val) => lower_byte = *val as u16,
                    None => {
                        panic!("Out of bounds access of code: {}", ip);
                    }
                }
            },
            None => panic!("AAAAAAAAAAAA"),
        }
        return  (higher_byte << 8 ) | lower_byte;

    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte().unwrap();
        let frame = self.getCurrentFunction().unwrap();
        let output: Value;
        match &frame.func{
            Some(fnc) => {
               match fnc.borrow().chunk.get_constant(index as usize) {
                    None => {
                        panic!("Out of bounds access of code: {}", frame.ip);
                    }
                    Some(val) => {output =  val.clone();} 
               }
            },
            None => {
                panic!("Invalid function");
            }
        }
        return output;
    }

    fn getCurrentFunction(&mut self) -> Option<&mut CallFrame>{
        self.frames.get_mut(self.frameCount-1)
    }
}
