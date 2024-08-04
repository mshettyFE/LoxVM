use crate::value::{Value, ValueArray};

#[derive(FromPrimitive)]
pub enum OpCode {
    OP_CONSTANT,
    OP_RETURN,
}

pub trait Disassemble {
    fn disassemble(&self, name: String) -> Result<(), String>;
}

pub struct Chunk {
    code: Vec<u8>,
    value_array: ValueArray,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self{
        Chunk {code: Vec::new(), value_array: ValueArray::new(), lines: Vec::new()}
    }

    pub fn write_chunk(&mut self, new_instr: u8, line: usize){
        self.code.push(new_instr);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, new_val: Value) ->usize{
        self.value_array.write_value(new_val);
        return self.value_array.get_count()-1; // returns position of start of opcode (hopefully).
    }


    pub fn get_count(&self) ->usize{
        self.code.len()
    }

  fn disassemble_instruction(&self, offset: usize) -> Result<usize, String>{
    print!("{}",format!("{:04}",offset));
    let found_opcode = self.code.get(offset);
    match found_opcode { // check if offset is out of bounds
        Some(opcode) => {
            match offset { // we know that offset is not OOB. Just need to check if it's 0 or not
                           // to determine line numbering
                0 => print!("{}", format!("{:4} ", self.lines[offset])),
                _ => print!("{}",format!("    | ")),
            }
           let oc = num::FromPrimitive::from_u8(*opcode);
            match oc { // make sure that u8 conversion went well
                Some(instr) => {
                    let new_offset = match instr{ // dispatch to different output based on instr
                        OpCode::OP_RETURN => self.simple_instruction("OP_RETURN".to_string(), offset)?,
                        OpCode::OP_CONSTANT => self.constant_instruction("OP_CONSTANT".to_string(), offset)?,
                    };
                    return Ok(new_offset);
                },
                None => {print!("Unknown opcode at {}", offset); return Ok(offset+1);}
           }
       },
        None => Err(format!("Couldn't access opcode at {}", offset)),
    }
  }

  fn simple_instruction(&self, name: String, offset: usize) -> Result<usize,String>{
    println!(" {}", name);
    return Ok(offset+1);
  }

  fn constant_instruction(&self, name: String, offset: usize) -> Result<usize,String> {
    let constant_index = self.code.get(offset+1);
    match constant_index {
        Some(const_index) => {
            match self.value_array.get_value(*const_index as usize) {
                Some (value) =>{
                    print!("{}", format!("{:<16} {:4} '", name, *const_index));
                    value.print_value();
                    print!("'\n");
                    return Ok(offset +2); // jump to after end of OP_CONSTANT
                 }
                None =>{
                    return Err(format!("Trying to access {} outside of value range", *const_index));
                }
            }
        }
        None => {
            println!("Trying to access {} outside of code range", offset+1);
            return Ok(offset+1);
        }
    }
  }

}


impl Disassemble for Chunk {
  fn disassemble(&self, name: String) -> Result<(), String>{
    println!("== {} ==", name);

    let mut offset: usize = 0;
    while offset < self.get_count() {
        offset = self.disassemble_instruction(offset)?;
    }
    Ok(())
  }
}



