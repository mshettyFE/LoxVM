#![allow(non_camel_case_types)]
use crate::value::{LoxFunction, UpvalueIndex, UpvalueType};

// Growing list of supported opcodes
#[derive(Clone, Debug)]
pub enum OpCode {
    // each entry corresponds to a single u8 unless noted otherwise
    OP_CONSTANT(usize),
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_GLOBAL(usize),
    OP_GET_LOCAL(usize),
    OP_CLOSE_UPVALUE,
    OP_GET_UPVALUE(usize),
    OP_SET_UPVALUE(usize),
    OP_DEFINE_GLOBAL(usize),
    OP_SET_GLOBAL(usize),
    OP_SET_LOCAL(usize),
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
    OP_JUMP(usize),
    OP_JUMP_IF_FALSE(usize),
    OP_LOOP(usize),
    OP_CALL(usize),
    OP_CLOSURE(usize,Vec<UpvalueIndex>),
    OP_RETURN,
}

impl OpCode{
    pub fn length(&self) -> usize{
        let out = match self{
            OpCode::OP_CONSTANT(_) => 2,
            OpCode::OP_GET_LOCAL(_) => 2,
            OpCode::OP_SET_LOCAL(_) => 2,
            OpCode::OP_GET_UPVALUE(_) => 2,
            OpCode::OP_SET_UPVALUE(_) => 2,
            OpCode::OP_DEFINE_GLOBAL(_) => 2,
            OpCode::OP_GET_GLOBAL(_) => 2,
            OpCode::OP_SET_GLOBAL(_) => 2,
            OpCode::OP_JUMP(_) => 2,
            OpCode::OP_JUMP_IF_FALSE(_) => 2,
            OpCode::OP_LOOP(_) => 2,
            OpCode::OP_CALL(_) => 2,
            OpCode::OP_CLOSURE(_,upvalues ) => {
                2+2*upvalues.len()
            }
            _ => 1
        };
        out
    }
}

#[derive(Clone)]
pub enum Constant{
    NUMBER(f64),
    STRING(String),
    FUNCTION(LoxFunction),
}

#[derive(Clone)]
pub struct Chunk {
    code: Vec<OpCode>, // holds the byte code for the chunk
    constant_array: Vec<Constant>, // hold the constants of the chunk
    lines: Vec<usize>, // for a given instruction, store the line where it was originated from
}

impl Chunk {
    pub fn new() -> Self{
        Chunk {code: Vec::new(), constant_array: Vec::new(), lines: Vec::new()}
    }

    pub fn write_chunk(&mut self, data: OpCode, line: usize){
        // add new data to  the chunk (not necessarily a single instruction. Could be part of an
        // instruction
        self.code.push(data);
        self.lines.push(line);
    }

    pub fn edit_chunk(&mut self, index: usize, new_data: OpCode){
        self.code[index] = new_data;
    }

    pub fn get_instr(&self, index: usize) -> Option<&OpCode>{
        // grab the start of an instruction
        self.code.get(index)
    }

    pub fn add_constant(&mut self, new_val: Constant) ->usize{
        self.constant_array.push(new_val);
        return self.constant_array.len()-1;
    }

    pub fn get_constant(&self, index: usize) -> Option<&Constant>{
        self.constant_array.get(index)
    }

    pub fn get_count(&self) ->usize{
        self.code.len()
    }

    pub fn get_line(&self, index: usize) -> Option<&usize>{
        self.lines.get(index)
    }

  pub fn disassemble_instruction(&self, code_index: usize, chunk_location: usize) -> Result<(), String>{
    // disassembles the code at a particular index in the chunk
    print!("{}",format!("{:04}", chunk_location));
    let found_opcode = self.code.get(code_index);
    match found_opcode { // check if offset is out of bounds
        None => Err(format!("Couldn't access opcode at {}", code_index)),
        Some(opcode) => {
            match code_index { // we know that offset is not OOB. Just need to check if it's 0 or not
                           // to determine line numbering
                0 => print!("{}", format!("{:4} ", self.lines[code_index])),
                _ => print!("{}",format!("    | ")),
            }
            match opcode { // make sure that u8 conversion went well
                    OpCode::OP_PRINT => self.simple_instruction("OP_PRINT")?,
                    OpCode::OP_CALL(index) => self.byte_instruction("OP_CALL", *index)?,
                    OpCode::OP_CLOSURE(constant_index, upvalues) => {
                        let closure_function = match self.constant_array.get(*constant_index).unwrap(){
                            Constant::FUNCTION(func) => func,
                            _ => panic!()
                        };

                        println!("{} {} {}", "OP_CLOSURE", *constant_index, closure_function.name);
                        let mut upval_index = 0;
                        for val in upvalues{
                            let local_name = match val.isLocal {
                                UpvalueType::UPVALUE => "upvalue",
                                UpvalueType::LOCAL => "local"
                            };
                            println!("{:04}    |     {} {}", chunk_location+2*upval_index, local_name, val.index); 
                            upval_index += 1;
                        }
                       return Ok(());
                    },
                    OpCode::OP_RETURN => self.simple_instruction("OP_RETURN")?,
                    OpCode::OP_NIL => self.simple_instruction("OP_NIL")?,
                    OpCode::OP_TRUE => self.simple_instruction("OP_TRUE")?,
                    OpCode::OP_FALSE => self.simple_instruction("OP_FALSE")?,
                    OpCode::OP_POP => self.simple_instruction("OP_POP")?,
                    OpCode::OP_CLOSE_UPVALUE => self.simple_instruction("OP_CLOSE_UPVALUE")?,
                    OpCode::OP_DEFINE_GLOBAL(index) => self.constant_instruction("OP_DEFINE_GLOBAL", *index)?,
                    OpCode::OP_GET_GLOBAL(index) => self.constant_instruction("OP_GET_GLOBAL", *index)?,
                    OpCode::OP_SET_GLOBAL(index) => self.constant_instruction("OP_SET_GLOBAL", *index)?,
                    OpCode::OP_GET_LOCAL(index) => self.byte_instruction("OP_GET_LOCAL", *index )?,
                    OpCode::OP_SET_LOCAL(index) => self.byte_instruction("OP_SET_LOCAL", *index )?,
                    OpCode::OP_GET_UPVALUE(index) => self.byte_instruction("OP_GET_UPVALUE", *index)?,
                    OpCode::OP_SET_UPVALUE(index) => self.byte_instruction("OP_SET_UPVALUE", *index)?,
                    OpCode::OP_JUMP(jump_length) => self.jump_instruction("OP_JUMP", true , *jump_length, chunk_location)?,
                    OpCode::OP_LOOP(jump_length) => self.jump_instruction("OP_LOOP", false, *jump_length, chunk_location)?,
                    OpCode::OP_JUMP_IF_FALSE(jump_length) => self.jump_instruction("OP_JUMP_IF_FALSE", true , *jump_length, chunk_location)?,
                    OpCode::OP_EQUAL => self.simple_instruction("OP_EQUAL")?,
                    OpCode::OP_GREATER => self.simple_instruction("OP_GREATER")?,
                    OpCode::OP_LESS => self.simple_instruction("OP_LESS")?,
                    OpCode::OP_NEGATE => self.simple_instruction("OP_NEGATE")?,
                    OpCode::OP_CONSTANT(index) => self.constant_instruction("OP_CONSTANT", *index)?,
                    OpCode::OP_ADD => self.simple_instruction("OP_ADD")?,
                    OpCode::OP_SUBTRACT => self.simple_instruction("OP_SUBTRACT")?,
                    OpCode::OP_MULTIPLY => self.simple_instruction("OP_MULTIPLY")?,
                    OpCode::OP_DIVIDE => self.simple_instruction("OP_DIVIDE")?,
                    OpCode::OP_NOT => self.simple_instruction("OP_NOT")?,
                }
                return Ok(());
                }
            }
        }

  fn simple_instruction(&self, name: &str) -> Result<(),String>{
    // format: Just the opcode
    println!(" {}", name);
    return Ok(())
  }

  fn constant_instruction(&self, name: &str, constant_index: usize) -> Result<(),String> {
    // format: OPCODE at offset, and then immediately afterwards, the index in the ValueArray
    // corresponding to the constant
            match self.constant_array.get(constant_index) {
                None =>  return Err(format!("Trying to access {} outside of value range", constant_index)),
                Some (value) =>{
                    print!("{}", format!("{:<16} {:4} '", name, constant_index));
                    match value{
                        Constant::NUMBER(num) => print!("{}", num),
                        Constant::STRING(str) => print!("{}", str),
                        Constant::FUNCTION(func) => print!("{}", func.name)
                    }
                    print!("'\n");
                    return Ok(()); // jump to after end of OP_CONSTANT
                }
            }
  }

  fn jump_instruction(&self, name: &str, sign: bool, jump: usize, initial_chunk_loc: usize) -> Result<(), String>{
      // format: name, offset, offset_to_jump_to
     let s: isize = match sign {
            true => 1,
            false => -1
      };
      let jump_offset = s * (jump as isize);
      println!("{} {} {}", name, initial_chunk_loc, initial_chunk_loc as isize + jump_offset);
      Ok(())
  }

  fn byte_instruction(&self, name: &str, value_index: usize) -> Result<(), String>{
    // format: OPCODE name value
    println!("{} {}", name, value_index);
    return Ok(());
  }

  pub fn disassemble(&self, name: String) -> Result<(), String>{
    // helper function to print out chunk contents
    println!("== {} ==", name);

    let mut offset: usize = 0;
    let mut lox_ref: usize = 0;
    while offset < self.get_count() {
        self.disassemble_instruction(offset, lox_ref);
//        self.disassemble_instruction(offset, offset);
        lox_ref += self.code.get(offset).unwrap().length();
        offset += 1;
    }
    Ok(())
  }
}
