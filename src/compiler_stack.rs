use crate::compiler::{Compiler, FunctionType};
// stack of compilers to allow functions to be implemented

pub struct CompilerStack{
    array: Vec<Compiler>
}

impl CompilerStack{
    pub fn new() -> Self{
        let mut a: Vec<Compiler> = Vec::new();
        a.push(Compiler::new(FunctionType::TYPE_SCRIPT, None));
        CompilerStack { array: a }
    }

    pub fn push(&mut self, val: Compiler){
        self.array.push(val);
   }

    pub fn pop(&mut self) -> Option<Compiler> {
        match self.array.pop() {
            Some(comp) => {
                return Some(comp)
            },
            None => return None
        }
   }

    pub fn peek_mut(&mut self) -> Option<&mut Compiler>{
        let cur_size = self.array.len()-1;
        self.array.get_mut( cur_size)
   }

}
