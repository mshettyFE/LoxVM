use std::collections::LinkedList;

use crate::object::ObjUpvalue;

pub struct OpenUpvaluesLL{
   head: std::collections::LinkedList<ObjUpvalue> 
}

impl OpenUpvaluesLL{
    pub fn new() -> Self{
        OpenUpvaluesLL{head: LinkedList::new()}
    }
}
