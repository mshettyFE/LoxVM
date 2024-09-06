use crate::value::Value;
use std::any::Any;

#[derive(Clone, PartialEq, Debug)]
pub enum ObjType{
    OBJ_STRING,
}

pub trait Obj {
    fn any(&self) -> &dyn Any;
    fn get_type(&self) -> ObjType; 
    fn print_obj(&self);
}

#[derive(Clone, Debug)]
pub struct LoxString{
   pub val: String, 
   pub hash: u64,
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
        let h = LoxString::gen_hash(new_val.clone());
        LoxString{val: new_val, hash: h,ttype: ObjType::OBJ_STRING}
    }

    fn gen_hash(val: String) -> u64{
        let mut hash: u64 = 2166136261;
        for byte in val.as_bytes(){
            hash ^= *byte as u64;
            hash = hash.wrapping_mul(16777619);
        }
        return hash;
    }
}
