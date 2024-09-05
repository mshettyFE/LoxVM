#![allow(non_camel_case_types)]
// wrapper around Rust HashMap which only supports LoxString insertions
// This is because we don't need a general hash map, and there are some
// intricacies when doing == which prevent me from just using the bare implementation

use std::{collections::{hash_map, HashMap}, hash::Hash};

use crate::{object::{LoxString, ObjType}, value::Value};
use std::cell::RefCell;
use std::rc::Rc;

pub struct LoxTable {
    hash_map: HashMap<u64, Value>,
}

impl LoxTable{
    pub fn new() -> Self{
        LoxTable{hash_map: HashMap::new()}
    }
    pub fn insert(&mut self, new_value: LoxString) {
        let new_val = Value::VAL_OBJ(Rc::new(RefCell::new(new_value.clone())));
        self.hash_map.insert(new_value.hash, new_val);
    }
    pub fn delete(&mut self, val_to_delete: LoxString){
        let hash = val_to_delete.hash; 
        self.hash_map.remove(&hash);
    }

    pub fn find(&self, key: LoxString) -> Option<Value>{
        let hash = key.hash;
        return self.hash_map.get(&hash).cloned();
   }

    fn FindString(&self, chars: String, hash: u64) -> Option<LoxString>{
        match self.hash_map.get(&hash) {
           Some(val) => {
                match val { 
                    Value::VAL_OBJ(pointer_stuff)=>{
                        match pointer_stuff.borrow().get_type() {
                           ObjType::OBJ_STRING => {
                               let ptr = pointer_stuff.borrow();
                               let output = ptr.any().downcast_ref::<LoxString>().unwrap();
                                if chars == output.val{
                                    return Some(output.clone());
                                }
                                return None;
                           } 
                        }
                    }
                   _ => {return None;}
                }
           },
           None => {return None},
        }
    }
}
