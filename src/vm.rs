#![allow(non_camel_case_types)]
use std::cell::RefCell;
use std::rc::Rc;

use crate::object::{LoxClosure, LoxFunction, NativeFn, Obj, ObjNative, ObjUpvalue};
use crate::{chunk::OpCode, object::LoxString, compiler::isFalsey, scanner::Scanner, DEBUG_TRACE_EXEC};
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
    pub closure: Option<Rc<RefCell<LoxClosure>>>,
    pub ip: usize, // ip relative to the current function (so 0 is the start of the current
                         // function, which has no relationship to other functions due to how the
                         // call stack is structured)
    pub starting_index: usize, // initial index into vm stack for this function
}

impl CallFrame {
    pub fn new(cls: Rc<RefCell<LoxClosure>>, new_starting_index: usize) -> Self{
        CallFrame{closure: Some(cls), ip: 0, starting_index: new_starting_index} 
    }
}

pub struct VM{
    frames: Vec<CallFrame>, // stores the function stack
    frameCount: usize, // Top stack pointer to frames
    stk: LoxStack, // value stack 
    globals: LoxTable,  // global vars
    parser: Parser, // Bundled here b/c Rust is paranoid
    upvalues: Vec<Rc<RefCell<ObjUpvalue>>> // linked list of upvalues allocated on the heap
}

pub fn clockNative(_argC: usize, _value_index: usize) -> Value{
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
        parser: Parser::new(),
        upvalues: Vec::new()
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
                self.stk.push(Value::VAL_OBJ(Rc::new(RefCell::new(LoxClosure::new(comp.function.unwrap()))).clone()));
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
                        let frame = self.getCurrentFunction().unwrap();
                        match &frame.closure {
                            Some(cls) => {
                                match &cls.borrow_mut().function{
                                    Some(fun) => {
                                        let _ = fun.chunk.disassemble_instruction(frame.ip);
                                    }
                                    None => {
                                        panic!("AAAAAAAAA");
                                    } 
                                }
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
                    self.getCurrentFunction().unwrap().ip -= offset as usize;
                }
                OpCode::OP_JUMP =>{ // unconditional jump
                    let offset = self.read_short(); // How many addresses do we need to jump ahead
                    self.getCurrentFunction().unwrap().ip += offset as usize;
                }
                OpCode::OP_JUMP_IF_FALSE => {
                    let offset = self.read_short();
                    if isFalsey(self.stk.peek(0).unwrap()) { // If top of stack if false, jump
                                                             // ahead
                        self.getCurrentFunction().unwrap().ip += offset as usize;
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
                    match self.read_constant(){
                        Value::VAL_OBJ(pointer_stuff) => {
                            let ptr = pointer_stuff.borrow();
                            let func = ptr.any().downcast_ref::<LoxFunction>().unwrap().clone();
                            let mut a   = LoxClosure::new(func);
                            for i in 0..a.upvalueCount{
                                let isLocal = self.read_byte();
                                let index = self.read_byte();
                                if  isLocal.unwrap() == 1 {
                                    let new_index = self.getCurrentFunction().unwrap().starting_index + index.unwrap() as usize;
                                    a.upvalues[i] = Some(self.captureUpvalue(new_index));
                                } else{
                                    match &self.getCurrentFunction().unwrap().closure{
                                        Some(c) => {
                                            a.upvalues[i] = c.borrow_mut().upvalues[i].clone(); 
                                        },
                                        None => {panic!("AAAA");}
                                    }
                                }
                            }
                            self.stk.push(Value::VAL_OBJ(Rc::new( RefCell::new(a)  )));
                         }
                        _ => {
                            panic!("AAAAAAAAAAAAAAAAA");
                        }
                   }
               },
               OpCode::OP_GET_UPVALUE => {
                    let slot = self.read_byte().unwrap();
                    match self.getCurrentFunction() {
                        Some(cf) => {
                            match &cf.closure {
                                Some(c) => {
                                    let new_val  = match c.borrow().upvalues.get(slot as usize-1){
                                        Some(loc) => {
                                            match loc{
                                                Some(x) => {
                                                    *x.clone()
                                                },
                                                None => panic!("AAAA")
                                            }
                                        },
                                        None => panic!("AAAA")
                                    };
                                    match new_val{
                                        ObjUpvalue::Open(idx) => {self.stk.push(self.stk.get(idx).unwrap());}
                                        ObjUpvalue::Closed(_) => !todo!()
                                    }
                                },
                                None => panic!("AAAA")
                            }
                        },
                        None => panic!("AAAA")
                   }
               },
               OpCode::OP_SET_UPVALUE => {
                    let new_val = self.stk.peek(0).unwrap();
                    let slot = self.read_byte().unwrap();
                    let mut index: Option<usize> = None;
                    match self.getCurrentFunction() {
                        Some(cf) => {
                            match & cf.closure {
                                Some(c) => {
                                    let mut temp = c.borrow_mut();
                                    match temp.upvalues.get_mut(slot as usize){
                                        Some(loc) => {
                                            match loc{
                                                Some(x) => {
                                                    match &mut **x{
                                                        ObjUpvalue::Open(idx) => index = Some(*idx),
//                                                        ObjUpvalue::Closed(val) => *val = new_val
                                                        ObjUpvalue::Closed(_) => todo!()
                                                    }
                                                },
                                                None => panic!("AAAA")
                                            }
                                        },
                                        None => panic!("AAAA")
                                    }
                                },
                                None => panic!("AAAA")
                            }
                        },
                        None => panic!("AAAA")
                   }
                    match index{
                        Some(idx) => self.stk.set(idx, new_val),
                        None => ()
                    }
               },
                OpCode::OP_RETURN => { // return from function
                    let result = self.stk.pop();
                    // clean up the stack to until the current function call gets erased
                    let current_pointer = self.getCurrentFunction().unwrap().starting_index;
                    while self.stk.size() > current_pointer{
                        self.stk.pop();
                    }
                    // Pop from the call stack
                    self.frameCount -= 1;
                    self.frames.pop();
                    // If at the base, then you need to exit from the program
                    if (self.frameCount == 0){
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
               OpCode::OP_GET_LOCAL => { // get local variable, and push to stack
                    let slot = match self.read_byte() {
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",err_msg))),
                    };
                    let cur_frame = self.getCurrentFunction().unwrap();
                    let index = cur_frame.starting_index + slot as usize +1;
                    self.stk.push(self.stk.get(index).unwrap());
               }
               OpCode::OP_SET_LOCAL => { // get location of variable in stack, then update value
                    let slot = match self.read_byte() {
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",err_msg))),
                    };
                    let cur_frame = self.getCurrentFunction().unwrap();
                    let index = cur_frame.starting_index + slot as usize+1;
//                    println!("ABS Index {}, starting {}, slot {}", index, cur_frame.starting_index, slot);
                    self.stk.set(index, self.stk.peek(0).unwrap());
               }
               OpCode::OP_GET_GLOBAL => { // get value of global variable from the global hash
                                          // table
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
               OpCode::OP_DEFINE_GLOBAL => { // Creating a new global variable 
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
               OpCode::OP_SET_GLOBAL => { // updating an existing variable
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
                        // doing floating point comparison
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
            Value::VAL_OBJ(obj_ptr) => {
                let obj = obj_ptr.borrow();
                match obj.get_type() {
                    crate::object::ObjType::OBJ_CLOSURE => {
                        return self.Call(obj_ptr.clone(), argCount);
                    },
                    crate::object::ObjType::OBJ_FUNCTION => {
                        return self.Call(obj_ptr.clone(), argCount);
                   },
                    crate::object::ObjType::OBJ_NATIVE => {
                        let fn_start = self.stk.size()- argCount as usize;
                        // execute the native function and  push the result onto the VM stack
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

    fn captureUpvalue(&mut self, local: usize) -> Box<ObjUpvalue>{
        let createdUpvalue = Box::new(ObjUpvalue::Open(local));
        return createdUpvalue;
    }

    fn Call(&mut self, closure_wrapper: Rc<RefCell<dyn Obj>> , argCount: u8) -> Result<(),String>{
        // This does runtime checking that the number of input arguments matches the arity of the
        // function
        self.frameCount += 1;
        if(self.frameCount == 255){
            return Err("Stack overflow.".to_string());
        }
        let closure = closure_wrapper.borrow();
        if argCount as usize !=
            closure.any().downcast_ref::<LoxClosure>().unwrap().function.clone().unwrap().arity {
            return Err(format!("Expected {} arguments but got {}.",closure.any().downcast_ref::<LoxFunction>().unwrap().arity, argCount));
        }
        // Adds a new frame to the call stack
        self.frames.push(
            CallFrame::new(Rc::new(RefCell::new(closure.any().downcast_ref::<LoxClosure>().unwrap().clone())),
            self.stk.size() - argCount as usize -1 )
        );
        return Ok(());
    } 

    fn read_byte(&mut self) -> Result<u8, String> {
        // read one instruction from the current chunk
        // also advance ip by 1
        let frame = self.getCurrentFunction();
        let f = frame.unwrap();
        let output: Result<u8,String>;
        match &f.closure {
            Some(cls) =>{
                match &cls.borrow().function{
                    Some(fnc) => {
                        match fnc.chunk.get_instr(f.ip) {
                            Some(val) => output = Ok(*val),
                            None => {
                                let err_msg = format!("Out of bounds access of code: {}", f.ip);
                                return Err(err_msg);
                            }
                        }
                        f.ip += 1;
                    },
                    None => panic!("AAAAAAAAAAAA"),
                }
            },
            None => panic!("AAAAAAAAA")
        }
       return output;
    }
 
    fn peek_stack(&mut self,  index :usize, expected: std::mem::Discriminant<Value>) -> Result<Value, String> {
        // check if top of stack matches the expected type
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
        // pretty printing runtime errors
        let err_msg = format!("{}\n",formatted_message);
        let frame = self.getCurrentFunction().unwrap();
        // ip points to the NEXT instruction to be executed, so need to decrement ip by 1
        let instruction = frame.ip - 1;
        let line: usize;
        match &frame.closure{
            Some(cls) => {
                match &cls.borrow().function{
                    Some(fnc) => {
                        match fnc.chunk.get_line(instruction) {
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
            },
            None => panic!("AAAAAAAA")
        }
        let fname = match &frame.closure{
            Some(cls) => {
                match &cls.borrow().function{
                    Some(function) => {
                        match &function.name{
                            Some(name) => name.val.clone(),
                            None => "script".to_string(),
                        }
                    },
                    None => "script".to_string()
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
                    match &frame.closure{
                        Some(cls) => {
                            match &cls.borrow().function{
                                None => panic!("AAAAAAAAAA"),
                                Some(function) => {
                                    let instr = frame.ip;
                                    let line_num = function.chunk.get_line(instr);
                                    stack_trace = format!("{}[line {}] in", stack_trace, line_num.unwrap());
                                    match &function.name{
                                        None => {
                                            stack_trace = format!("{} script\n",stack_trace );
                                        }
                                        Some(string) => {
                                            stack_trace = format!("{} {}()\n",stack_trace, string.val.clone() );
                                        }
                                    }
                                }
                            }
                        },
                        None => panic!("AAAAAAAAAA")
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
        // The pushes and pops are weird garbage collector things that aren't really necessary in
        // a Rust-based VM
        self.stk.push(Value::VAL_OBJ(Rc::new( RefCell::new(name.clone()))));
        self.stk.push(Value::VAL_OBJ(Rc::new( RefCell::new(ObjNative::new(function)))));
        self.globals.insert( name, self.stk.get(1).unwrap());
        self.stk.pop();
        self.stk.pop();
    }

    fn read_short(&mut self) -> u16{
        // read the next two bytes in the chunk, incrementing the ip as needed
        self.getCurrentFunction().unwrap().ip += 2;
        let ip = self.getCurrentFunction().unwrap().ip;
        let higher_byte: u16;
        let lower_byte: u16;
        match &self.getCurrentFunction().unwrap().closure {
            Some(cls) => {
                match &cls.borrow().function {
                    Some(fnc) => {
                        match fnc.chunk.get_instr(ip-2) {
                            Some(val) => higher_byte = *val as u16,
                            None => {
                                panic!("Out of bounds access of code: {}", ip);
                            }
                        }
                        match fnc.chunk.get_instr(ip-1){
                            Some(val) => lower_byte = *val as u16,
                            None => {
                                panic!("Out of bounds access of code: {}", ip);
                            }
                        }
                    },
                    None => panic!("AAAAAAAAAAAA"),
                }
            }
            None => panic!("AAAAAAAAA")
       }
        return  (higher_byte << 8 ) | lower_byte;

    }

    fn read_constant(&mut self) -> Value {
        // read a value from the chunk and return the value. Don't mess with the stack
        let index = self.read_byte().unwrap();
        let frame = self.getCurrentFunction().unwrap();
        let output: Value;
        match &frame.closure{
            Some(cls) => {
               match &cls.borrow().function{
                   Some(fnc) => {
                       match fnc.chunk.get_constant(index as usize) {
                           None => {panic!("Out of bounds access of code: {}", frame.ip);}
                           Some(val) => {output =  val.clone();} 
                       }
                   },
                   None => panic!("AAAA"),
               }
            }
            None => {
                panic!("Invalid function");
            }
        }
        return output;
    }

    fn getCurrentFunction(&mut self) -> Option<&mut CallFrame>{
        // helper function to get the top CallFrame
        self.frames.get_mut(self.frameCount-1)
    }
}
