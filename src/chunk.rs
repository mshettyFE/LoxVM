#![allow(non_camel_case_types)]

use crate::value::Value;

// Growing list of supported opcodes
#[derive(FromPrimitive)]
pub enum OpCode {
    // each entry corresponds to a single u8 unless noted otherwise
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_GLOBAL,
    OP_GET_LOCAL,
    OP_CLOSE_UPVALUE,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_SET_LOCAL,
    OP_ADD,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_RETURN,
}

#[derive(Clone)]
pub struct Chunk {
    code: Vec<u8>, // holds the byte code for the chunk
    value_array: Vec<Value>, // hold the values of the chunk as floats
    lines: Vec<usize>, // for a given instruction, store the line where it was originated from
}

impl Chunk {
    pub fn new() -> Self{
        Chunk {code: Vec::new(), value_array: Vec::new(), lines: Vec::new()}
    }

    pub fn write_chunk(&mut self, data: u8, line: usize){
        // add new data to  the chunk (not necessarily a single instruction. Could be part of an
        // instruction
        self.code.push(data);
        self.lines.push(line);
    }

    pub fn edit_chunk(&mut self, index: usize, new_data: u8){
        self.code[index] = new_data;
    }

    pub fn get_instr(&self, index: usize) -> Option<&u8>{
        // grab the start of an instruction
        self.code.get(index)
    }

    pub fn add_constant(&mut self, new_val: Value) ->usize{
        self.value_array.push(new_val);
        return self.value_array.len()-1; // returns position of start of opcode (hopefully).
    }

    pub fn get_constant(&self, index: usize) -> Option<&Value>{
        self.value_array.get(index)
    }

    pub fn get_count(&self) ->usize{
        self.code.len()
    }

    pub fn get_line(&self, index: usize) -> Option<&usize>{
        self.lines.get(index)
    }

  pub fn disassemble_instruction(&self, offset: usize) -> Result<usize, String>{
    // disassembles the code at a particular index in the chunk
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
                    OpCode::OP_PRINT => self.simple_instruction("OP_PRINT".to_string(), offset)?,
                    OpCode::OP_CALL => self.byte_instruction("OP_CALL".to_string(), offset)?,
                    OpCode::OP_CLOSURE => {
                        let mut cur_offset = offset; 
                        cur_offset += 1;
                        let constant = self.code.get(cur_offset).unwrap();
                        cur_offset += 1;
                        print!("{} {} ", "OP_CLOSURE", constant);
                        self.get_constant(*constant as usize).unwrap().print_value();
                        println!("");
                        match self.get_constant(*constant as usize).unwrap(){
                            Value::VAL_FUNCTION(func) => {
                                for _ in 0..func.upvalueCount{
                                    let isLocal = self.get_instr(cur_offset).unwrap();
                                    let local_name = match isLocal {1 => "local", 0 => "upvalue", _ => panic!()};
                                    cur_offset += 1;
                                    let index = self.get_instr(cur_offset).unwrap();
                                    cur_offset += 1;
                                    let v = self.get_constant(*index as usize).unwrap();
                                    println!("{:04}    |     {} {}", cur_offset-2, local_name, index);
                                } 
                            },
                            _ => panic!()
                        }
                        return Ok(cur_offset);
    
                    },
                    OpCode::OP_RETURN => self.simple_instruction("OP_RETURN".to_string(), offset)?,
                    OpCode::OP_NIL => self.simple_instruction("OP_NIL".to_string(), offset)?,
                    OpCode::OP_TRUE => self.simple_instruction("OP_TRUE".to_string(), offset)?,
                    OpCode::OP_FALSE => self.simple_instruction("OP_FALSE".to_string(), offset)?,
                    OpCode::OP_POP => self.simple_instruction("OP_POP".to_string(), offset)?,
                    OpCode::OP_CLOSE_UPVALUE => self.simple_instruction("OP_CLOSE_UPVALUE".to_string(), offset)?,
                    OpCode::OP_DEFINE_GLOBAL => self.constant_instruction("OP_DEFINE_GLOBAL".to_string(), offset)?,
                    OpCode::OP_GET_GLOBAL => self.constant_instruction("OP_GET_GLOBAL".to_string(), offset)?,
                    OpCode::OP_SET_GLOBAL => self.constant_instruction("OP_SET_GLOBAL".to_string(), offset)?,
                    OpCode::OP_GET_LOCAL => self.byte_instruction("OP_GET_LOCAL".to_string(), offset)?,
                    OpCode::OP_SET_LOCAL => self.byte_instruction("OP_SET_LOCAL".to_string(),offset)?,
                    OpCode::OP_GET_UPVALUE => self.byte_instruction("OP_SET_UPVALUE".to_string(),offset)?,
                    OpCode::OP_SET_UPVALUE => self.byte_instruction("OP_SET_UPVALUE".to_string(),offset)?,
                    OpCode::OP_JUMP => self.jump_instruction("OP_JUMP".to_string(), true , offset)?,
                    OpCode::OP_LOOP => self.jump_instruction("OP_LOOP".to_string(), false, offset)?,
                    OpCode::OP_JUMP_IF_FALSE => self.jump_instruction("OP_JUMP_IF_FALSE".to_string(), true , offset)?,
                    OpCode::OP_EQUAL => self.simple_instruction("OP_EQUAL".to_string(), offset)?,
                    OpCode::OP_GREATER => self.simple_instruction("OP_GREATER".to_string(), offset)?,
                    OpCode::OP_LESS => self.simple_instruction("OP_LESS".to_string(), offset)?,
                    OpCode::OP_NEGATE => self.simple_instruction("OP_NEGATE".to_string(), offset)?,
                    OpCode::OP_CONSTANT => self.constant_instruction("OP_CONSTANT".to_string(), offset)?,
                    OpCode::OP_ADD => self.simple_instruction("OP_ADD".to_string(), offset)?,
                    OpCode::OP_SUBTRACT => self.simple_instruction("OP_SUBTRACT".to_string(), offset)?,
                    OpCode::OP_MULTIPLY => self.simple_instruction("OP_MULTIPLY".to_string(), offset)?,
                    OpCode::OP_DIVIDE => self.simple_instruction("OP_DIVIDE".to_string(), offset)?,
                    OpCode::OP_NOT => self.simple_instruction("OP_NOT".to_string(), offset)?,
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
            match self.value_array.get(*const_index as usize) {
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

  fn jump_instruction(&self, name: String, sign: bool, offset: usize) -> Result<usize, String>{
      // format: name, offset, offset_to_jump_to
      let high_byte: u16 = (*self.code.get(offset+1).unwrap()  as u16) << (8 as u16);
      let low_byte: u16 = *self.code.get(offset+2).unwrap()  as u16;
      let jump: u16 = high_byte | low_byte;
      let s: isize = match sign {
            true => 1,
            false => -1
      };
      let jump_offset = s * (jump as isize);
      let val: usize = (offset as i64 + 3 + (jump_offset as i64)) as usize;
      println!("{} {} {}", name,offset, val);
      Ok(offset +3)
  }

  fn byte_instruction(&self, name: String, offset: usize) -> Result<usize, String>{
    // format: OPCODE name value
    let slot = match self.code.get(offset+1){
        Some(val) => val,
        None => {
            println!("Trying to access {} outside of code range", offset+1);
            return Ok(offset+1);
        } 
    };
    println!("{} {}", name, slot);
    return Ok(offset +2);
  }

  pub fn disassemble(&self, name: String) -> Result<(), String>{
    // helper function to print out chunk contents
    println!("== {} ==", name);

    let mut offset: usize = 0;
    while offset < self.get_count() {
        offset = self.disassemble_instruction(offset)?;
    }
    Ok(())
  }
}
