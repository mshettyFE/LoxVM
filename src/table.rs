#![allow(non_camel_case_types)]
// wrapper around Rust HashMap which only supports LoxString insertions
// This is because we don't need a general hash map, and there are some
// intricacies when doing == which prevent me from just using the bare implementation

use std::collections::HashMap;

use crate::value::Value;

pub struct LoxTable {
    hash_map: HashMap<String, Value>,
}

impl LoxTable{
    pub fn new() -> Self{
        LoxTable{hash_map: HashMap::new()}
    }
    pub fn insert(&mut self, key: String, val: Value) -> Option<Value> {
        self.hash_map.insert(key, val)
    }
    pub fn delete(&mut self, val_to_delete: String){
        self.hash_map.remove(&val_to_delete);
    }

    pub fn find(&self, key: String) -> Option<Value>{
        return self.hash_map.get(&key).cloned();
   }

}
