#![allow(non_camel_case_types)]
use std::any::Any;

use crate::chunk::Chunk;
use crate::value::Value;

#[derive(Clone,Debug, PartialEq)]
pub enum ObjType{
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_CLOSURE,
    OBJ_UPVALUE
}

// OBJ trait which acts kind of like a "base class"
pub trait Obj {
    fn any(&self) -> &dyn Any;
    fn get_type(&self) -> ObjType; 
    fn print_obj(&self);
}


// Native function, which takes in the number of parameters expected, and the starting index into the stack
pub type NativeFn = fn(usize, usize) -> Value;

pub struct ObjNative{
    pub function: NativeFn
}

impl ObjNative {
    pub fn new(func: NativeFn) -> Self{
        ObjNative{function: func}
    }
}

impl Obj for ObjNative{
    fn any(&self) -> &dyn Any{
        self
    }
    fn get_type(&self) -> ObjType{
       return ObjType::OBJ_NATIVE 
    }
    
    fn print_obj(&self){
        print!("<native fn>"); 
    }
}

#[derive(Clone)]
pub struct LoxClosure{
    pub function: Option<LoxFunction>,
    pub upvalues: Vec<Option<Box<ObjUpvalue>>>,
    pub upvalueCount: usize
}

impl Obj for LoxClosure{
    fn any(&self) -> &dyn Any{
        self
    }
    fn get_type(&self) -> ObjType{
       return ObjType::OBJ_CLOSURE 
    }
    
    fn print_obj(&self){
        match &self.function {
            Some(v) => v.print_obj(),
            None => print!("CLOSURE_NONE")
        }
    }
}

impl LoxClosure{
    pub fn new(new_func: LoxFunction) -> Self{
        let mut a: Vec<Option<Box<ObjUpvalue>>> = Vec::new();
        for _ in 0..new_func.upvalueCount{
            a.push(None);
        }
        LoxClosure{function: Some(new_func.clone()),
        upvalues: a,
        upvalueCount: new_func.upvalueCount}

    }
}

// LoxFunction
#[derive(Clone)]
pub struct LoxFunction{
    pub arity: usize, // the number of expected arguments to the function
    pub chunk: Chunk, // the associated bytecode of the function
    pub name: Option<LoxString>, // the name of the function
    pub upvalueCount: usize                             // number of upvalues the function needs access to
}

impl Obj for LoxFunction {
    fn get_type(&self) -> ObjType {
        return ObjType::OBJ_FUNCTION
    }
    fn any(&self) -> &dyn Any {
        // utilize with downcast ref in conjunction with get_type
        // https://stackoverflow.com/questions/52247927/convert-between-a-reference-to-a-trait-and-a-struct-that-implements-that-trait-i
        self
    }
    fn print_obj(&self){
        match &self.name {
            Some(v) => print!("<fn {}>", v.val),
            None => print!("<script>")
        }
    }
}

impl LoxFunction {
    pub fn new(n_arity: usize, new_name: Option<LoxString>) -> Self{
        LoxFunction{arity: n_arity, chunk: Chunk::new(), name: new_name, upvalueCount: 0}
    }
}

// Defines a LoxString, which is a wrapper around the Rust String to interface with the rest of the
// VM
#[derive(Clone, Debug)]
pub struct LoxString{
   pub val: String, 
   pub hash: u64,
}

impl Obj for LoxString{
    fn get_type(&self) -> ObjType {
        return ObjType::OBJ_STRING
    }
    fn any(&self) -> &dyn Any {
        // utilize with downcast ref in conjunction with get_type
        // https://stackoverflow.com/questions/52247927/convert-between-a-reference-to-a-trait-and-a-struct-that-implements-that-trait-i
        self
    }
    fn print_obj(&self){
        print!("{}", self.val)
    }
}

impl LoxString{
    pub fn new(new_val: String) -> LoxString{
        let h = LoxString::gen_hash(new_val.clone());
        LoxString{val: new_val, hash: h}
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
pub enum ObjUpvalue{
    Open(usize),
    Closed(Value)
}

impl Obj for ObjUpvalue{
    fn get_type(&self) -> ObjType {
        return ObjType::OBJ_UPVALUE
    }
    fn any(&self) -> &dyn Any {
        // utilize with downcast ref in conjunction with get_type
        // https://stackoverflow.com/questions/52247927/convert-between-a-reference-to-a-trait-and-a-struct-that-implements-that-trait-i
        self
    }
    fn print_obj(&self){
        print!("upvalue")
    }
}

impl ObjUpvalue{
    pub fn is_open(&self) -> bool{
        match self {
            ObjUpvalue::Open(_) => true,
            ObjUpvalue::Closed(_) => false,
        }
    }
    pub fn is_open_with_index(&self, index: usize) -> bool{
        match self {
            ObjUpvalue::Open(idx) => *idx==index,
            ObjUpvalue::Closed(_) => false,
        }
    }
}
