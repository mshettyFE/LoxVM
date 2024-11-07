use crate::value::*;
use std::collections::HashMap;

pub type heapID = u64;

enum heapData{
    String(String),
    Function(LoxFunction),
    Native(NativeFn),
    Closure(LoxClosure),
    Class(LoxClass)
}

pub trait HeapSize{
    fn heapSize(&self) -> u64;
}

impl heapData{
    pub fn as_str(&self) -> Option<&String>{ if let heapData::String(s) = self {Some(s)} else {None}}
    pub fn as_function(&self) -> Option<&LoxFunction>{ if let heapData::Function(s) = self {Some(s)} else {None}}
    pub fn as_native(&self) -> Option<&NativeFn>{ if let heapData::Native(s) = self {Some(s)} else {None}}
    pub fn as_closure(&self) -> Option<&LoxClosure>{ if let heapData::Closure(s) = self {Some(s)} else {None}}
    pub fn as_class(&self) -> Option<&LoxClass>{ if let heapData::Class(s) = self {Some(s)} else {None}}

   fn heapSize(&self) -> u64 {std::mem::size_of_val(self) as u64}
}

struct heapVal{
    is_marked:bool,
    data: heapData
}

pub struct Heap{
   curCapacity: u64,
   nextGc: u64,
   NextID: heapID,
   values: HashMap<heapID, heapVal> 
}

impl Heap{
    pub fn new() -> Self{
        Heap{
            curCapacity: 0,
            nextGc: 1024*1024,
            NextID: 0,
            values: HashMap::new()
        }
    }

    pub fn manage_str(&mut self, s: String) -> heapID{self.add_data(heapData::String(s))}
    pub fn manage_function(&mut self, s: LoxFunction) -> heapID{self.add_data(heapData::Function(s))}
    pub fn manage_native(&mut self, s: NativeFn) -> heapID{self.add_data(heapData::Native(s))}
    pub fn manage_closure(&mut self, s: LoxClosure) -> heapID{self.add_data(heapData::Closure(s))}
    pub fn manage_class(&mut self, s: LoxClass) -> heapID{self.add_data(heapData::Class(s))}

    fn generate_id(&mut self) -> heapID {
        let a = self.NextID;
        self.NextID += 1;
        return a;
    }

    fn add_data(&mut self, new_data: heapData) -> heapID{
        self.curCapacity += new_data.heapSize();
        let id = self.generate_id();
        self.values.insert(id, heapVal{is_marked: false, data:new_data });
        id
    }

    pub fn get_str(&self, id: heapID) -> &String{self.values.get(&id).unwrap().data.as_str().unwrap()}
    pub fn get_function(&self, id: heapID) -> &LoxFunction{self.values.get(&id).unwrap().data.as_function().unwrap()}
    pub fn get_native(&self, id: heapID) -> &NativeFn{self.values.get(&id).unwrap().data.as_native().unwrap()}
    pub fn get_closure(&self, id: heapID) -> &LoxClosure{self.values.get(&id).unwrap().data.as_closure().unwrap()}
    pub fn get_class(&self, id: heapID) -> &LoxClass{self.values.get(&id).unwrap().data.as_class().unwrap()}

}
