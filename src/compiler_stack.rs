use crate::compiler::{Compiler, FunctionType};
// shamelessly adapted from https://rust-unofficial.github.io/too-many-lists/second-final.html
// stack of compilers to allow functions to be implemented

pub struct CompilerStack{
    head: CSLink
}

type CSLink = Option<Box<Node>>;

struct Node{
    elem: Compiler,
    next: CSLink

}

impl CompilerStack {
    pub fn new() -> Self{
        let mut a =  CompilerStack{head: None};
        a.push(Compiler::new(FunctionType::TYPE_SCRIPT, None));
        a
    }
    
    pub fn push(&mut self, val: Compiler){
        let new_node = Box::new(Node {
            elem: val,
            next: self.head.take(),
        });
        self.head = Some(new_node);
    }

    pub fn pop(&mut self) -> Option<Compiler> {
         self.head.take().map(|node| {
            self.head = node.next;
            node.elem
        })
    }

    pub fn peek_mut(&mut self) -> Option<&mut Compiler>{
        self.head.as_mut().map(|node| {
            &mut node.elem
        })
    }
}
