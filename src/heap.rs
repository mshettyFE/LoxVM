use crate::value::*;
use std::collections::HashMap;

pub type HeapID = u64;

enum HeapData{
    String(String),
    Function(LoxFunction),
    Native(NativeFn),
    Closure(LoxClosure),
    Class(LoxClass),
    Instance(LoxInstance),
    BoundMethod(LoxBoundMethod)
}

pub trait HeapSize{
    fn heapSize(&self) -> u64;
}

impl HeapData{
    pub fn as_str(&self) -> Option<&String>{ if let HeapData::String(s) = self {Some(s)} else {None}}
    pub fn as_function(&self) -> Option<&LoxFunction>{ if let HeapData::Function(s) = self {Some(s)} else {None}}
    pub fn as_native(&self) -> Option<&NativeFn>{ if let HeapData::Native(s) = self {Some(s)} else {None}}
    pub fn as_closure(&self) -> Option<&LoxClosure>{ if let HeapData::Closure(s) = self {Some(s)} else {None}}
    pub fn as_class(&self) -> Option<&LoxClass>{ if let HeapData::Class(s) = self {Some(s)} else {None}}
    pub fn as_instance(&self) -> Option<&LoxInstance>{ if let HeapData::Instance(s) = self {Some(s)} else {None}}
    pub fn as_bound_method(&self) -> Option<&LoxBoundMethod>{ if let HeapData::BoundMethod(s) = self {Some(s)} else {None}}

    pub fn as_mut_str(&mut self) -> Option<&mut String>{ if let HeapData::String(s) = self {Some(s)} else {None}}
    pub fn as_mut_function(&mut self) -> Option<&mut LoxFunction>{ if let HeapData::Function(s) = self {Some(s)} else {None}}
    pub fn as_mut_native(&mut self) -> Option<&mut NativeFn>{ if let HeapData::Native(s) = self {Some(s)} else {None}}
    pub fn as_mut_closure(&mut self) -> Option<&mut LoxClosure>{ if let HeapData::Closure(s) = self {Some(s)} else {None}}
    pub fn as_mut_class(&mut self) -> Option<&mut LoxClass>{ if let HeapData::Class(s) = self {Some(s)} else {None}}
    pub fn as_mut_instance(&mut self) -> Option<&mut LoxInstance>{ if let HeapData::Instance(s) = self {Some(s)} else {None}}
    pub fn as_mut_bound_method(&mut self) -> Option<&mut LoxBoundMethod>{ if let HeapData::BoundMethod(s) = self {Some(s)} else {None}}

   fn heapSize(&self) -> u64 {std::mem::size_of_val(self) as u64}
}

struct HeapVal{
// if I do garbage collection, would need this
//    is_marked:bool,
    data: HeapData
}

pub struct Heap{
   curCapacity: u64,
//   nextGc: u64,
   NextID: HeapID,
   values: HashMap<HeapID, HeapVal> 
}

impl Heap{
    pub fn new() -> Self{
        Heap{
            curCapacity: 0,
//            nextGc: 1024*1024,
            NextID: 0,
            values: HashMap::new()
        }
    }

    pub fn manage_str(&mut self, s: String) -> HeapID{self.add_data(HeapData::String(s))}
    pub fn manage_function(&mut self, s: LoxFunction) -> HeapID{self.add_data(HeapData::Function(s))}
    pub fn manage_native(&mut self, s: NativeFn) -> HeapID{self.add_data(HeapData::Native(s))}
    pub fn manage_closure(&mut self, s: LoxClosure) -> HeapID{self.add_data(HeapData::Closure(s))}
    pub fn manage_class(&mut self, s: LoxClass) -> HeapID{self.add_data(HeapData::Class(s))}
    pub fn manage_instance(&mut self, s: LoxInstance) -> HeapID{self.add_data(HeapData::Instance(s))}
    pub fn manage_bound_method(&mut self, s: LoxBoundMethod) -> HeapID{self.add_data(HeapData::BoundMethod(s))}

    fn generate_id(&mut self) -> HeapID {
        let a = self.NextID;
        self.NextID += 1;
        return a;
    }

    fn add_data(&mut self, new_data: HeapData) -> HeapID{
        self.curCapacity += new_data.heapSize();
        let id = self.generate_id();
//        self.values.insert(id, HeapVal{is_marked: false, data:new_data });
        self.values.insert(id, HeapVal{data:new_data });
        id
    }

    pub fn get_str(&self, id: HeapID) -> &String{self.values.get(&id).unwrap().data.as_str().unwrap()}
    pub fn get_function(&self, id: HeapID) -> &LoxFunction{self.values.get(&id).unwrap().data.as_function().unwrap()}
    pub fn get_native(&self, id: HeapID) -> &NativeFn{self.values.get(&id).unwrap().data.as_native().unwrap()}
    pub fn get_closure(&self, id: HeapID) -> &LoxClosure{self.values.get(&id).unwrap().data.as_closure().unwrap()}
    pub fn get_class(&self, id: HeapID) -> &LoxClass{self.values.get(&id).unwrap().data.as_class().unwrap()}
    pub fn get_instance(&self, id: HeapID) -> &LoxInstance{self.values.get(&id).unwrap().data.as_instance().unwrap()}
    pub fn get_bound_method(&self, id: HeapID) -> &LoxBoundMethod{self.values.get(&id).unwrap().data.as_bound_method().unwrap()}

    pub fn get_mut_str(&mut self, id: HeapID) -> &mut String{self.values.get_mut(&id).unwrap().data.as_mut_str().unwrap()}
    pub fn get_mut_function(&mut self, id: HeapID) -> &mut LoxFunction{self.values.get_mut(&id).unwrap().data.as_mut_function().unwrap()}
    pub fn get_mut_native(&mut self, id: HeapID) -> &mut NativeFn{self.values.get_mut(&id).unwrap().data.as_mut_native().unwrap()}
    pub fn get_mut_closure(&mut self, id: HeapID) -> &mut LoxClosure{self.values.get_mut(&id).unwrap().data.as_mut_closure().unwrap()}
    pub fn get_mut_class(&mut self, id: HeapID) -> &mut LoxClass{self.values.get_mut(&id).unwrap().data.as_mut_class().unwrap()}
    pub fn get_mut_instance(&mut self, id: HeapID) -> &mut LoxInstance{self.values.get_mut(&id).unwrap().data.as_mut_instance().unwrap()}
    pub fn get_mut_bound_method(&mut self, id: HeapID) -> &mut LoxBoundMethod{self.values.get_mut(&id).unwrap().data.as_mut_bound_method().unwrap()}

}
