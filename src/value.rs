#![allow(non_camel_case_types)]

use crate::chunk::*;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone,Debug, PartialEq)]
pub enum LoxType{
    NIL,
    BOOL,
    NUMBER,
    STRING,
    FUNCTION,
    NATIVE,
    CLOSURE
}

pub enum Upvalue{
    Open(usize),
    Closed(Value)
}

#[derive(Clone, PartialEq)]
pub struct LoxString{
    pub name: String,
    pub hash: u64
}

impl LoxString{
    pub fn new(val: String) -> Self{
        let h = LoxString::gen_hash(val.clone());
        LoxString{name: val, hash: h}
    }

    fn gen_hash(val: String) -> u64{
        // simple hash function called "FNV-1a". 
        let mut hash: u64 = 2166136261;
        for byte in val.as_bytes(){
            hash ^= *byte as u64;
            hash = hash.wrapping_mul(16777619);
        }
        return hash;
    }
}

#[derive(Clone)]
pub struct LoxFunction{
    pub arity: usize,
    pub name: LoxString,
    pub chunk: Chunk
}

impl LoxFunction{
    pub fn new( new_arity: usize, new_name: LoxString) -> Self{
        LoxFunction{arity: new_arity, name: new_name, chunk: Chunk::new()}
    }
}

#[derive(Clone)]
pub struct LoxClosure{
    pub function: LoxFunction,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>
}

pub type NativeFn = fn(usize, usize) -> Value;

// Represents the possible values which the VM can hold
#[derive(Clone)]
pub enum Value {
    VAL_NIL,
    VAL_BOOL(bool),
    VAL_NUMBER(f64),
    VAL_STRING(LoxString),
    VAL_FUNCTION(LoxFunction),
    VAL_NATIVE(NativeFn),
    VAL_CLOSURE(LoxClosure)
}

impl std::fmt::Debug for Value{
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       self.print_value();
       Ok(()) 
    }
}

impl Value {
    pub fn print_value(&self){
        match self{
            Value::VAL_NIL => print!("NIL"),
            Value::VAL_BOOL(v) => match v{
                true => print!("true"),
                false => print!("false")
            },
            Value::VAL_NUMBER(num) => print!("{:<.5}", num),
            Value::VAL_STRING(name) => print!("{}", name.name),
            Value::VAL_FUNCTION(func) => print!("{}", func.name.name),
            Value::VAL_NATIVE(_) => print!("<native fn>"),
            Value::VAL_CLOSURE(closure) => { print!("{}",closure.function.name.name)} 
       }
    }

    pub fn get_type(&self) -> LoxType{
        match(self){
            Value::VAL_NIL => LoxType::NIL,
            Value::VAL_BOOL(_) => LoxType::BOOL,
            Value::VAL_NUMBER(_) => LoxType::NUMBER,
            Value::VAL_STRING(_) => LoxType::STRING,
            Value::VAL_FUNCTION(_) => LoxType::FUNCTION,
            Value::VAL_NATIVE(_) => LoxType::NATIVE,
            Value::VAL_CLOSURE(_) => LoxType::CLOSURE
        }
    }
}

pub fn valuesEqual(a: Value, b: Value) -> bool{
    let type_a = a.get_type();
    let type_b = b.get_type();

    if type_a==type_b {
        match type_a{
            LoxType::NIL => return true,
            LoxType::BOOL => {
                if let Value::VAL_BOOL(a_val) = a{
                    if let Value::VAL_BOOL(b_val) = b{
                        return a_val == b_val;
                    }
                }
            }
            LoxType::NUMBER => {
                if let Value::VAL_NUMBER(a_val) = a{
                    if let Value::VAL_NUMBER(b_val) = b{
                        return a_val == b_val;
                    }
                } 
            }
            LoxType::STRING => {
                if let Value::VAL_STRING(a_val) = a{
                    if let Value::VAL_STRING(b_val) = b{
                        return a_val == b_val;
                    }
                } 
            }
            _ => {
                panic!("Can't compare  these types");
            }
        }
    }
    return false;
} 
