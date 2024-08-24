#![allow(non_camel_case_types)]

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Value {
    VAL_NIL,
    VAL_BOOL(bool),
    VAL_NUMBER(f64),
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
        }
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
