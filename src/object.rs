use crate::value::Value;
use std::any::Any;

#[derive(Clone, PartialEq)]
pub enum ObjType{
    OBJ_STRING,
}

pub trait Obj {
    fn any(&self) -> &dyn Any;
    fn get_type(&self) -> ObjType; 
    fn print_obj(&self);
}

pub struct LoxString{
   pub val: String, 
   ttype: ObjType
}

impl Obj for LoxString{
    fn get_type(&self) -> ObjType {
        return self.ttype.clone()
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
        LoxString{val: new_val, ttype: ObjType::OBJ_STRING}
    }
}
