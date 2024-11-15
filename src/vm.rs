#![allow(non_camel_case_types)]

use std::collections::HashMap;
use crate::heap::Heap;
use crate::{chunk::OpCode, compiler::isFalsey, scanner::Scanner, DEBUG_TRACE_EXEC};
use std::rc::Rc;
use std::cell::RefCell;
use crate:: stack::LoxStack;
use crate::value::*;
use crate::compiler::*;

use core::{fmt, panic};
use std::time::{SystemTime, UNIX_EPOCH};
pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR(String),
    INTERPRET_RUNTIME_ERROR(String),
}

// the current function being executed, as well as where in the VM the function starts at
pub struct CallFrame{
    pub closure: LoxClosure,
    pub ip: usize, // ip relative to the current function (so 0 is the start of the current
                         // function, which has no relationship to other functions due to how the
                         // call stack is structured)
    pub starting_index: usize, // initial index into vm stack for this function
    pub lox_index: usize
}

impl CallFrame {
    pub fn new(cls: LoxClosure, new_starting_index: usize) -> Self{
        CallFrame{closure: cls, ip: 0, starting_index: new_starting_index, lox_index: 0} 
    }
}

pub struct VM{
    frames: Vec<CallFrame>, // stores the function stack
    stk: LoxStack, // value stack 
    gc: Heap,       // heap allocated things
    globals: HashMap<String, Value>,  // global vars
    parser: Parser, // Bundled here b/c Rust is paranoid
    upvalues: Vec<Rc<RefCell<Upvalue>>>, // list of pointers to upvalues
    initString: String
}

pub fn clockNative(_argC: usize, _value_index: usize) -> Value{
    let time =     SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64();
    Value::VAL_NUMBER(time)
}

impl VM {
    pub fn new() -> Self{
    let mut x = VM{
        frames: Vec::new(),
        globals: HashMap::new(),
        stk: LoxStack::new(),
        gc: Heap::new(),
        parser: Parser::new(),
        upvalues: Vec::new(),
        initString: "init".to_string()
        };

        // native functions get defined here
        x.defineNative("clock".to_string(), clockNative);
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
                let id = self.gc.manage_closure( LoxClosure { function: comp.function, upvalues: Vec::new() } );
                self.stk.push(Value::VAL_CLOSURE( id ));
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
             // get current instruction to run in the current chunk
             let instruction  = match self.read_byte(){
                Ok(val) => val,
                Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",err_msg))),
             };
              if *DEBUG_TRACE_EXEC.get().unwrap(){
               self.stk.print(&self.gc);
               let frame = self.getCurrentFrame();
               frame.closure.function.chunk.disassemble_instruction(frame.ip-1, frame.ip-1).unwrap();
             }
            {
                let frame = self.getCurrentFrame();
                frame.lox_index += instruction.length();
             }
              match instruction { //finally, dispatch to the correct opcode
                OpCode::OP_PRINT => {
                    match self.stk.pop(){
                        Some(v) => {
                            v.print_value(&self.gc);
                            println!();
                        }
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Stack is empty")))}
                     }
                 }
                OpCode::OP_LOOP(offset) => {
                    self.getCurrentFrame().ip -= offset+1;
                }
                OpCode::OP_JUMP(offset) =>{ // unconditional jump
                    self.getCurrentFrame().ip += offset-1;
                }
                OpCode::OP_JUMP_IF_FALSE(offset) => {
                    if isFalsey(self.stk.peek(0).unwrap()) { // If top of stack if false, jump ahead
                        self.getCurrentFrame().ip += offset-1;
                    }
                },
                OpCode::OP_CALL(argCount) => { // execute the function
                    match self.callValue(self.stk.peek(argCount).unwrap(), argCount){
                        Err(str) => {return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",str)))}
                        Ok(()) => {()}
                    }
                },
                OpCode::OP_CLOSURE(function_index, upvalues) => {
                    if let Value::VAL_FUNCTION(id) = self.read_constant(function_index){
                    let new_upvalues = upvalues.iter().map(
                            |uval| match uval.isLocal{
                                UpvalueType::UPVALUE => {self.getCurrentFrame().closure.upvalues[uval.index].clone() },
                                UpvalueType::LOCAL => {
                                    if let Some(upval) = self.find_open_upvals(uval.index){
                                        upval
                                    } else{
                                        let abs_index = self.getCurrentFrame().starting_index+uval.index-1;
                                        let upval = Rc::new(RefCell::new(Upvalue::Open(abs_index)));
                                        self.upvalues.push(upval.clone());
                                        upval
                                    }
                                }

                                
                            }
                        ).collect();
                        let func = self.gc.get_function(id);
                        let closure_id = self.gc.manage_closure(LoxClosure{function: func.clone(), upvalues:  new_upvalues });
                        let new_c = Value::VAL_CLOSURE(closure_id);
                        self.stk.push(new_c);
                    } else {panic!()}
              },
               OpCode::OP_GET_UPVALUE(index) => {
                   let upvalue_copy = self.getCurrentFrame().closure.upvalues[index].borrow().clone();
                   match upvalue_copy{
                     Upvalue::Open(idx) => {
                         self.stk.push(self.stk.get( idx).unwrap())
                     },
                     Upvalue::Closed(val) => {
                         self.stk.push(val.clone())
                     }
                   }
              },
               OpCode::OP_SET_UPVALUE(index) => {
                   let val = match self.stk.peek(0){
                        Some(v) => {
                            v
                       }
                        None => panic!()
                    };
                    let uv = self.getCurrentFrame().closure.upvalues[index].borrow().clone();
                    match uv{
                        Upvalue::Open(idx) => {
                            self.stk.set( idx, val)
                        },
                        Upvalue::Closed(_) => {
                            self.getCurrentFrame().closure.upvalues[index] = Rc::new(RefCell::new(Upvalue::Closed(val)))
                        }
                    }
              },
              OpCode::OP_CLOSE_UPVALUE => {
                  self.closeUpvalue(self.stk.size()-1);
                  self.stk.pop();
             }

                OpCode::OP_CLASS(index) => {
                    if let Value::VAL_STRING(str_id) =  self.read_constant(index){
                        let class_name = self.gc.get_str(str_id);
                        let class_id = self.gc.manage_class(LoxClass{name: class_name.clone(), methods: HashMap::new()});
                        self.stk.push(Value::VAL_CLASS(class_id));
                    } else {panic!()}
                },

                OpCode::OP_GET_PROPERTY(index) => {
                    if let Value::VAL_INSTANCE(id) = self.stk.peek(0).unwrap() {
                        if let Value::VAL_STRING(str_id) = self.read_constant(index){
                            let inst = self.gc.get_instance(id);
                            let name = self.gc.get_str(str_id);
                            match inst.fields.get(name){
                                Some(field_val) => {
                                    self.stk.pop();
                                    self.stk.push(field_val.clone());
                                },
                                None => {
                                    match self.bindMethod(inst.klass.clone(), name.clone()){
                                        Ok(()) => {},
                                        Err(str) => return InterpretResult::INTERPRET_RUNTIME_ERROR(str)
                                    }
                                }
                            }
                       } else {panic!()}
                    } else {
                        return InterpretResult::INTERPRET_RUNTIME_ERROR(
                                    self.formatRunTimeError(format_args!("Only instances have properties")))
                    }
                },

                OpCode::OP_SET_PROPERTY(index) =>{
                    
                    let name = match  self.read_constant(index){
                        Value::VAL_STRING(str_id) => self.gc.get_str(str_id).clone(),
                        _ => panic!()
                    };

                    if let Value::VAL_INSTANCE(id) = self.stk.peek(1).unwrap() {
                        {
                            let inst = self.gc.get_mut_instance(id);
                            inst.fields.insert(name.clone(),self.stk.peek(0).unwrap());
                            let val = self.stk.pop().unwrap();
                            self.stk.pop();
                            self.stk.push(val);
                        }
                    }
                    else {
                        return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Only instances have fields")))
                    }
                }

                OpCode::OP_INHERIT => {
                    let superclass = match self.stk.peek(1).unwrap(){
                        Value::VAL_CLASS(id) => self.gc.get_class(id),
                        _ => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("superclass must be a class")))
                    };
                   self.copyMethods(superclass.clone());
                   self.stk.pop(); 
                }

                OpCode::OP_METHOD(index) =>{
                    if let Value::VAL_STRING(str_id) =  self.read_constant(index) {
                        let method_name = self.gc.get_str(str_id);
                        self.defineMethod(method_name.clone());
                    }
                },

                OpCode::OP_INVOKE(constant_index, argCount ) => {
                    let method = match self.read_constant(constant_index){
                        Value::VAL_STRING(id) => self.gc.get_str(id),
                        _ => panic!()
                    };
                    match self.invoke(method.clone(), argCount) {
                        Ok(()) => {},
                        Err(str) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",str))) 
                    }
                },
                OpCode::OP_SUPER_INVOKE(constant_index, argCount ) => {
                    let method = match self.read_constant(constant_index){
                        Value::VAL_STRING(id) => self.gc.get_str(id),
                        _ => panic!()
                    };
                    let super_class = match self.stk.pop().unwrap(){
                        Value::VAL_CLASS(id) => self.gc.get_class(id),
                        _ => panic!()
                    };

                    match self.invokeFromClass(super_class.clone(), method.clone(), argCount) {
                        Ok(()) => {},
                        Err(str) => return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}",str))) 
                    }
                },
                OpCode::OP_RETURN => { // return from function
                    let result = self.stk.pop();
                    for idx in self.getCurrentFrame().starting_index..self.stk.size(){
                        self.closeUpvalue(idx);
                    }
                    // clean up the stack to until the current function call gets erased
                    let current_pointer = self.getCurrentFrame().starting_index;
                    while self.stk.size() > current_pointer{
                        self.stk.pop();
                    }
                    // Pop from the call stack
                    self.frames.pop();
                    // If at the base, then you need to exit from the program
                    if self.frames.len() == 0 {
                        self.stk.pop(); // remove the stray NIL value from the stack
                        return InterpretResult::INTERPRET_OK
                    }
                    // add the return value to the VM value stack
                    self.stk.push(result.unwrap());
                },
                OpCode::OP_CONSTANT(constant_index) => { // add constant to stack
                  let constant: Value = self.read_constant(constant_index);
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
               OpCode::OP_GET_LOCAL(index) => { // get local variable, and push to stack
                   let index = self.getCurrentFrame().starting_index + index;
                    self.stk.push(self.stk.get(index).unwrap());
               }
               OpCode::OP_SET_LOCAL(index) => { // get location of variable in stack, then update value
                   let index = self.getCurrentFrame().starting_index + index;
                    self.stk.set(index, self.stk.peek(0).unwrap());
               }
               OpCode::OP_GET_GLOBAL(name_index) => { // get value of global variable from the global hash
                                          // table
                    let slot = self.read_constant(name_index);
                    let name = match slot {
                        Value::VAL_STRING(id) => self.gc.get_str(id),
                        _ => panic!()
                    };
                   let wrapped_val = self.globals.get(name);
                    match wrapped_val {
                        Some(value) => {
                            self.stk.push(value.clone());
                        }
                        None => {
                            return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Undefined variable {}.",name.clone())));
                        }
                     }
               }
               OpCode::OP_DEFINE_GLOBAL(name_index) => { // Creating a new global variable 
                    let name: Value = self.read_constant(name_index);
                    match name {
                        Value::VAL_STRING(str_id) => {
                               let value = self.stk.peek(0).unwrap();
                               let str = self.gc.get_str(str_id);
                               self.globals.insert(str.clone(), value);
                        },
                        _ => { return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Tried accessing a global")))}

                    } 
                    self.stk.pop();
               }
               OpCode::OP_SET_GLOBAL(name_index) => { // updating an existing variable
                    let name: Value = self.read_constant(name_index);
                    match name {
                        Value::VAL_STRING(str_id) => {
                               let value = self.stk.peek(0).unwrap();
                               let key = self.gc.get_str(str_id);
                               if self.globals.insert(key.to_string(), value).is_none(){
                                    return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("Undefined variable {}.", key.clone())))
                               }
                        },
                        _ => { return InterpretResult::INTERPRET_RUNTIME_ERROR(self.formatRunTimeError(format_args!("{}","Tried accessing a global")));}
                    } 
               }

               OpCode::OP_GET_SUPER(name_index) =>{
                    let name: Value = self.read_constant(name_index);
                    match name {
                        Value::VAL_STRING(str_id) => {
                            let super_name = self.gc.get_str(str_id);
                            let super_class = match self.stk.pop().unwrap() {
                                Value::VAL_CLASS(id) => self.gc.get_class(id),
                                _ => panic!()
                            };
                            match self.bindMethod(super_class.clone(), super_name.clone()){
                                Ok(()) => {},
                                Err(str) => return InterpretResult::INTERPRET_RUNTIME_ERROR(str)
                            }
                       },
                        _ => panic!() 
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
                                if let Value::VAL_STRING(a_val_id) = a{
                                    if let Value::VAL_STRING(b_val_id) = b{
                                        let a_val = self.gc.get_str(a_val_id);
                                        let b_val = self.gc.get_str(b_val_id);
                                        let new_str = format!("{}{}", b_val, a_val);
                                        let id = self.gc.manage_str(new_str);
                                        Value::VAL_STRING(id)
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
                               let output = match instruction {
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

    fn callClassMethod(&mut self, cls: LoxClass, instance_index: usize, argCount: usize) -> Result<(), String>{
            self.stk.set(instance_index, Value::VAL_INSTANCE(self.gc.manage_instance( LoxInstance::new(cls.clone()) )));
            let closure = match cls.methods.get(&self.initString) {
                    Some(Value::VAL_CLOSURE(id)) =>  Some(self.gc.get_closure(*id)),
                    _ => None
            }; 
            match closure {
                    Some(cls) => return self.Call(cls.clone(), argCount),
                    None => 
                        if argCount == 0 {
                            return Ok(())
                        } else {
                            return Err(self.formatRunTimeError(format_args!("Expected 0 arguments but got {}.", argCount)));
                        }
            }
    }

    fn callValue(&mut self, callee: Value, argCount: usize)-> Result<(),String>{
        // This is a wrapper around Call. It does type checking on the input Value to make sure
        // it's a callable
        match callee {
            Value::VAL_BOUND_METHOD(id) => {
                let bm = self.gc.get_bound_method(id);
                self.stk.set(self.stk.size()-argCount-1, Value::VAL_INSTANCE(bm.receiver.clone()) );
                return self.Call(bm.method.clone(), argCount);
            }
            Value::VAL_CLASS(id) =>{
                  let class = self.gc.get_class(id);
                  let instance_index = self.stk.size()-argCount-1;
                  self.callClassMethod(class.clone(), instance_index, argCount)
                }
            Value::VAL_CLOSURE(id) => {
                let cls = self.gc.get_closure(id);
                return self.Call( cls.clone(), argCount);
            },
            Value::VAL_NATIVE(native_func) => {
                        let fn_start = self.stk.size()- argCount as usize;
                        // execute the native function and  push the result onto the VM stack
                        let result = native_func(argCount as usize, fn_start);
                        while self.stk.size() > fn_start {
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

    fn invoke(&mut self, name: String, argCount: usize) -> Result<(), String>{
        let receiver = self.stk.peek(argCount).unwrap();
        match receiver {
            Value::VAL_INSTANCE(id) => {
                let inst = self.gc.get_instance(id);
                match inst.fields.get(&name){
                    Some(val) => {
                        let stk_idx = self.stk.size()-argCount-1;
                        self.stk.set(stk_idx, val.clone());
                        return self.callValue(self.stk.get(stk_idx).unwrap(), argCount);
                    },
                    None => {
                        return self.invokeFromClass(inst.klass.clone(), name, argCount);
                    }
                }
            },
            _ => {
                return Err(self.formatRunTimeError(format_args!("Only instances have methods")));
            }
        }
    }

    fn invokeFromClass(&mut self, class: LoxClass, name: String, argCount: usize) -> Result<(), String>{
        match class.methods.get(&name) {
            Some(method) => {
                match method {
                    Value::VAL_CLOSURE(id ) => {
                        let closure = self.gc.get_closure(*id);
                        return self.Call(closure.clone(), argCount);
                    }
                    _ => panic!()
                }
            },
            None => {
                return Err( self.formatRunTimeError( format_args!("Undefined property {}", name) ));
            }
        } 
    }

    fn bindMethod(&mut self, klass: LoxClass, name: String) -> Result<(), String>{
        let method = match klass.methods.get(&name){
            Some(value) => value,
            None => {
                let str = self.formatRunTimeError(format_args!("Undefined property {}", name));
                return Err(str)
            }
        };

        let cls =  match method {
            Value::VAL_CLOSURE(id) => self.gc.get_closure(*id),
            _ => panic!()
        };
        match self.stk.peek(0).unwrap(){
            Value::VAL_INSTANCE(id) => {
                let bm_id = self.gc.manage_bound_method(
                LoxBoundMethod::new(id, cls.clone()));
                self.stk.pop();
                self.stk.push(Value::VAL_BOUND_METHOD(bm_id));
            },
            _ => panic!()
        }
        return Ok(());
    }

    fn closeUpvalue(&mut self, index: usize){
        let value = match self.stk.get(index){
            Some(v) => v,
            None => return 
        };
        for upval in &self.upvalues{
            match upval.borrow().get_index(){
                Some(cur_idx) => {
                    if cur_idx> index {
                        break;
                    }
                },
                None => ()
            }
            if upval.borrow().is_open_with_index(index) {
                upval.replace(Upvalue::Closed(value.clone()));
            }
        }

        self.upvalues.retain(|u| u.borrow().is_open());
    }

    fn Call(&mut self, closure: LoxClosure , argCount: usize) -> Result<(),String>{
        // This does runtime checking that the number of input arguments matches the arity of the
        // function
        if self.frames.len()+1 == 255 {
            return Err("Stack overflow.".to_string());
        }
            if argCount as usize != closure.function.arity {
                    return Err(format!("Expected {} arguments but got {}.", closure.function.arity, argCount));
        }
        // Adds a new frame to the call stack
            self.frames.push( CallFrame::new(closure, self.stk.size() - argCount as usize -1 )); 
       return Ok(());
    } 

    fn read_byte(&mut self) -> Result<OpCode, String> {
        // read one instruction from the current chunk
        // also advance ip by 1
        let f = self.getCurrentFrame();

        let output = match f.closure.function.chunk.get_instr(f.ip){
                Some(val) => Ok(val.clone()),
                None => {
                  let err_msg = format!("Out of bounds access of code: {}", f.ip);
                  Err(err_msg)
                } 
        };
        f.ip += 1;
        return output;
    }

     fn read_constant(&mut self, index: usize) -> Value {
        let frame = self.getCurrentFrame();
        match frame.closure.function.chunk.get_constant(index as usize).expect("Out of bound error").clone(){
           crate::chunk::Constant::NUMBER(num) => Value::VAL_NUMBER(num),
           crate::chunk::Constant::STRING(str) => {
               let id = self.gc.manage_str(str);
               Value::VAL_STRING(id)
           },
           crate::chunk::Constant::FUNCTION(func) => {
               let id = self.gc.manage_function(func);
               Value::VAL_FUNCTION(id)
            }
        }
    }

     fn find_open_upvals(&self,index: usize) -> Option<Rc<RefCell<Upvalue>>>{
        for upval in self.upvalues.iter().rev() {
            if upval.borrow().is_open_with_index(index) {
                return Some(upval.clone());
            }        
        }
        None
     }

    fn formatRunTimeError(&mut self, formatted_message: fmt::Arguments) -> String{
        // pretty printing runtime errors
        let err_msg = format!("{}\n",formatted_message);
        let frame = self.getCurrentFrame();
        // ip points to the NEXT instruction to be executed, so need to decrement ip by 1
        let instruction = frame.ip - 1;
        println!("Current Instr: {}", instruction);
        let line: usize;
        match frame.closure.function.chunk.get_line(instruction){
                    Some(val) => line = val.clone(),
                    None => panic!()
        }
        let line_err = format!("[line {}] in {}\n", line,  frame.closure.function.name);

        // stack trace
        let mut cur_frame = self.frames.len()-1;
        let mut stack_trace = "".to_string();
        loop {
            let frame =  self.frames.get_mut(cur_frame).unwrap();
            stack_trace = format!("{} [line {}] in {}\n", stack_trace, frame.closure.function.chunk.get_line(frame.ip).unwrap(), frame.closure.function.name);
            if cur_frame == 0 {break;}
            cur_frame -= 1;
        }

        self.stk.reset();
        return format!("{}{}{}", line_err, err_msg, stack_trace);
    }

    fn defineNative(&mut self, name: String, function: NativeFn){
        // The pushes and pops are weird garbage collector things that aren't really necessary in
        // a Rust-based VM
        let id = self.gc.manage_str(name.clone());
        self.stk.push(Value::VAL_STRING(id));
        self.stk.push(Value::VAL_NATIVE(function));
        self.globals.insert( name, self.stk.get(1).unwrap());
        self.stk.pop();
        self.stk.pop();
    }

    fn defineMethod(&mut self, name: String){
        let method = self.stk.peek(0).unwrap();
        let klass = match self.stk.peek(1){
            Some(Value::VAL_CLASS(id)) => {
                self.gc.get_mut_class(id)
            },
            _ => panic!()
        };
        klass.methods.insert(name, method);
        self.stk.pop();
    }

    pub fn copyMethods(&mut self, superclass: LoxClass){
        let subclass = match self.stk.peek(0).unwrap(){
            Value::VAL_CLASS(id) => self.gc.get_mut_class(id),
            _ => panic!()
        };

        subclass.methods = superclass.methods.clone();
    }

    fn getCurrentFrame(&mut self) -> &mut CallFrame{
        // helper function to get the top CallFrame
        let length = self.frames.len();
        self.frames.get_mut(length-1).unwrap()
    }
}
