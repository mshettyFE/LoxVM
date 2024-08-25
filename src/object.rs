#[derive(Clone, PartialEq, PartialOrd)]
pub enum ObjType{
    OBJ_STRING,
}

#[derive(Clone, PartialEq, PartialOrd)]
pub struct Obj{
    ttype: ObjType,
}
