#![allow(non_camel_case_types)]
use crate::value::{Value, ValueArray};

// Growing list of supported opcodes
#[derive(FromPrimitive)]
pub enum OpCode {
    // each entry corresponds to a single u8
    OP_CONSTANT, // format: OPCODE, ValueArrayIndex
    // format of arithmetic ops: 
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NEGATE, // format:
    OP_RETURN, // format: OPCODE
}

pub struct Chunk {
    code: Vec<u8>, // holds the byte code for the chunk
    value_array: ValueArray, // hold the values of the chunk as floats
    lines: Vec<usize>, // for a given instruction, store the line where it was originated from
}

impl Chunk {
    pub fn new() -> Self{
        Chunk {code: Vec::new(), value_array: ValueArray::new(), lines: Vec::new()}
    }

    pub fn write_chunk(&mut self, data: u8, line: usize){
        // add new data to  the chunk (not necessarily a single instruction. Could be part of an
        // instruction
        self.code.push(data);
        self.lines.push(line);
    }

    pub fn get_instr(&self, index: usize) -> Option<&u8>{
        // grab the start of an instruction
        self.code.get(index)
    }

    pub fn add_constant(&mut self, new_val: Value) ->usize{
        self.value_array.write_value(new_val);
        return self.value_array.get_count()-1; // returns position of start of opcode (hopefully).
    }

    pub fn get_constant(&self, index: usize) -> Option<&Value>{
        self.value_array.get_value(index)

    }


    pub fn get_count(&self) ->usize{
        self.code.len()
    }

  pub fn disassemble_instruction(&self, offset: usize) -> Result<usize, String>{
    print!("{}",format!("{:04}",offset));
    let found_opcode = self.code.get(offset);
    match found_opcode { // check if offset is out of bounds
        None => Err(format!("Couldn't access opcode at {}", offset)),
        Some(opcode) => {
            match offset { // we know that offset is not OOB. Just need to check if it's 0 or not
                           // to determine line numbering
                0 => print!("{}", format!("{:4} ", self.lines[offset])),
                _ => print!("{}",format!("    | ")),
            }
            let oc = num::FromPrimitive::from_u8(*opcode);
            match oc { // make sure that u8 conversion went well
                // emite error if conversion failed, then continue
                None => {print!("Unknown opcode at {}", offset); return Ok(offset+1);},
                Some(instr) => {
                let new_offset = match instr{ // dispatch to different output based on instr
                    OpCode::OP_RETURN => self.simple_instruction("OP_RETURN".to_string(), offset)?,
                    OpCode::OP_NEGATE => self.simple_instruction("OP_NEGATE".to_string(), offset)?,
                    OpCode::OP_CONSTANT => self.constant_instruction("OP_CONSTANT".to_string(), offset)?,
                    OpCode::OP_ADD => self.simple_instruction("OP_ADD".to_string(), offset)?,
                    OpCode::OP_SUBTRACT => self.simple_instruction("OP_SUBTRACT".to_string(), offset)?,
                    OpCode::OP_MULTIPLY => self.simple_instruction("OP_MULTIPLY".to_string(), offset)?,
                    OpCode::OP_DIVIDE => self.simple_instruction("OP_DIVIDE".to_string(), offset)?,
                    };
                return Ok(new_offset);
                }
            }
        }
    }
  }

  fn simple_instruction(&self, name: String, offset: usize) -> Result<usize,String>{
    // format: Just the opcode
    println!(" {}", name);
    return Ok(offset+1);
  }

  fn constant_instruction(&self, name: String, offset: usize) -> Result<usize,String> {
    // format: OPCODE at offset, and then immediately afterwards, the index in the ValueArray
    // corresponding to the constant
    let constant_index = self.code.get(offset+1);
    match constant_index {
        None => {
            println!("Trying to access {} outside of code range", offset+1);
            return Ok(offset+1);
        },
        Some(const_index) => {
            match self.value_array.get_value(*const_index as usize) {
                None =>  return Err(format!("Trying to access {} outside of value range", *const_index)),
                Some (value) =>{
                    print!("{}", format!("{:<16} {:4} '", name, *const_index));
                    value.print_value();
                    print!("'\n");
                    return Ok(offset +2); // jump to after end of OP_CONSTANT
                }
            }
        }
    }
  }

  pub fn disassemble(&self, name: String) -> Result<(), String>{
    println!("== {} ==", name);

    let mut offset: usize = 0;
    while offset < self.get_count() {
        offset = self.disassemble_instruction(offset)?;
    }
    Ok(())
  }

}
