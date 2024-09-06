#![allow(non_camel_case_types)]
// wrapper around Rust HashMap which only supports LoxString insertions
// This is because we don't need a general hash map, and there are some
// intricacies when doing == which prevent me from just using the bare implementation

use std::{collections::HashMap};

use crate::{object::LoxString, value::Value};

pub struct LoxTable {
    hash_map: HashMap<u64, Value>,
}

impl LoxTable{
    pub fn new() -> Self{
        LoxTable{hash_map: HashMap::new()}
    }
    pub fn insert(&mut self, key: LoxString, val: Value) -> Option<Value> {
        self.hash_map.insert(key.hash, val)
    }
    pub fn delete(&mut self, val_to_delete: LoxString){
        let hash = val_to_delete.hash; 
        self.hash_map.remove(&hash);
    }

    pub fn find(&self, key: LoxString) -> Option<Value>{
        let hash = key.hash;
        return self.hash_map.get(&hash).cloned();
   }

    pub fn print(&self){
        for (key, value) in &self.hash_map {
            println!("{:#?}: {:#?}", key, value);
        } 
    }

}
