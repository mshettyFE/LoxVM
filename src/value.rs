#![allow(non_camel_case_types)]

use crate::chunk::*;
use crate::heap::*;
use std::collections::HashMap;
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
    CLOSURE,
    CLASS,
    INSTANCE
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UpvalueType{
    UPVALUE,
    LOCAL
}

#[derive(Clone, Copy, Debug)]
pub struct UpvalueIndex{
    pub index: usize,
    pub isLocal: UpvalueType
}

#[derive(Clone)]
pub enum Upvalue{
    Open(usize),
    Closed(Value)
}

impl Upvalue{
    pub fn is_open(&self) -> bool{
        match self{
            Upvalue::Open(_) => true,
            _ => false
        }
    }

    pub fn get_index(&self) -> Option<usize>{
        match self{
            Upvalue::Open(idx) => {Some(*idx)},
            Upvalue::Closed(_) => None
        }
    }

    pub fn is_open_with_index(&self, index: usize) -> bool{
        match self{
            Upvalue::Open(idx) => return *idx == index,
            Upvalue::Closed(_) => false
        }
    }
}

#[derive(Clone)]
pub struct LoxFunction{
    pub arity: usize,
    pub name: String,
    pub chunk: Chunk,
    pub upvalueCount: usize
}

impl LoxFunction{
    pub fn new( new_arity: usize, new_name: String) -> Self{
        LoxFunction{arity: new_arity, name: new_name, chunk: Chunk::new(), upvalueCount: 0}
    }
}

#[derive(Clone)]
pub struct LoxClosure{
    pub function: LoxFunction,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>
}

pub type NativeFn = fn(usize, usize) -> Value;

#[derive(Clone,Debug)]
pub struct LoxClass{
    pub name: String
}

#[derive(Clone,Debug)]
pub struct LoxInstance{
    pub klass: LoxClass,
    pub fields: HashMap<String, Value>
}

impl LoxInstance{
    pub fn new(cls: LoxClass) -> Self{
        LoxInstance{
            klass: cls,
            fields: HashMap::new()
        }
    }
}

// Represents the possible values which the VM can hold
#[derive(Clone,Debug)]
pub enum Value {
    VAL_NIL,
    VAL_BOOL(bool),
    VAL_NUMBER(f64),
    VAL_STRING(heapID),
    VAL_FUNCTION(heapID),
    VAL_NATIVE(NativeFn),
    VAL_CLOSURE(heapID),
    VAL_CLASS(heapID),
    VAL_INSTANCE(heapID)
}

impl Value {
    pub fn print_value(&self, heap: &Heap){
        match self{
            Value::VAL_NIL => print!("NIL"),
            Value::VAL_BOOL(v) => match v{
                true => print!("true"),
                false => print!("false")
            },
            Value::VAL_NUMBER(num) => print!("{:<.5}", num),
            Value::VAL_STRING(id) => print!("{}", heap.get_str(*id)),
            Value::VAL_FUNCTION(id) => print!("<fn {}>", heap.get_function(*id).name ),
            Value::VAL_NATIVE(_) => print!("<native fn>"),
            Value::VAL_CLOSURE(id) => { print!("{}", heap.get_closure(*id).function.name )},
            Value::VAL_CLASS(id) => { print!("{}", heap.get_class(*id).name )},
            Value::VAL_INSTANCE(id) => { print!("{} instance", heap.get_instance(*id).klass.name )},
       }
    }

    pub fn get_type(&self) -> LoxType{
        match self{
            Value::VAL_NIL => LoxType::NIL,
            Value::VAL_BOOL(_) => LoxType::BOOL,
            Value::VAL_NUMBER(_) => LoxType::NUMBER,
            Value::VAL_STRING(_) => LoxType::STRING,
            Value::VAL_FUNCTION(_) => LoxType::FUNCTION,
            Value::VAL_NATIVE(_) => LoxType::NATIVE,
            Value::VAL_CLOSURE(_) => LoxType::CLOSURE,
            Value::VAL_CLASS(_) => LoxType::CLASS,
            Value::VAL_INSTANCE(_) => LoxType::INSTANCE
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
