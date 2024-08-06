#![allow(non_camel_case_types)]
use crate::scanner::*;

pub struct Compiler{
    scanner: Scanner,
}

impl Compiler{
  pub fn new(src: String) -> Self{
    Compiler{scanner: Scanner::new(src)}
  }

  pub fn compile(&mut self) -> bool{
    let mut cur_line:usize = 0;
    loop {
        let token = self.scanner.scanToken(); 
        if token.line != cur_line{
            print!("{:4}",token.line);
            cur_line = token.line;
        } else{
            print!("  | ");
        }
        print!("{:?} {}", token.ttype, token.start);

        match token.ttype {
            TokenType::TOKEN_EOF => break,
            _ => println!("Unimplemented"),
        }
    }
  return true; 
  }
}
