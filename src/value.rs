#![allow(non_camel_case_types)]

use crate::object::*;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Value {
    VAL_NIL,
    VAL_BOOL(bool),
    VAL_NUMBER(f64),
    VAL_OBJ(Obj),
}

impl Value {
    pub fn print_value(&self){
        match self{
            Value::VAL_NIL => {print!("NIL")}
            Value::VAL_BOOL(truth_val) => {
               match truth_val{
                    true => {print!("true")},
                    false => {print!("false")}
               } 
            }
            Value::VAL_NUMBER(num) => {
                print!("{:<.5}", num);
            }
            Value::VAL_OBJ(objt) =>{
                
            }
        }
    }
}

pub fn valuesEqual(a: Value, b: Value) -> bool{
    if matches!(a, Value::VAL_NIL) && matches!(b, Value::VAL_NIL){
        return true;
    } 
    // default statements in following chain are unreachable
    if matches!(a, Value::VAL_BOOL(..)) && matches!(b, Value::VAL_BOOL(..)){
        let a_val = match a {Value::VAL_BOOL(val) => val, _ => false};
        let b_val = match b {Value::VAL_BOOL(val) => val, _ => false};
        return a_val == b_val;
    } else if matches!(a, Value::VAL_NUMBER(..)) && matches!(b, Value::VAL_NUMBER(..)){
        let a_val = match a {Value::VAL_NUMBER(val) => val, _ => 0.0};
        let b_val = match b {Value::VAL_NUMBER(val) => val, _ => 0.0};
        return a_val == b_val;
    } else{
        return false;
    } 
}

// stores the values in a vector for quick lookup
pub struct ValueArray{
    values: Vec<Value>,
}

impl ValueArray{
    pub fn new() -> Self{
        ValueArray {values: Vec::new()}
    }

    pub fn write_value(&mut self, new_val: Value){
        self.values.push(new_val);
    }

    pub fn get_value(&self, index: usize) -> Option<&Value>{
        return self.values.get(index);
    }

    pub fn get_count(&self) ->usize{
        self.values.len()
    }
}
