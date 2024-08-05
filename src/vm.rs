use crate::{chunk::{Chunk, OpCode}, DEBUG_TRACE_EXEC};
use crate:: stack::LoxStack;
use crate::value;

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR(String),
    INTERPRET_RUNTIME_ERROR(String),
}

pub struct VM {
    chunk: Box<Chunk>, // currently executing chunk
    ip: usize, // index into code section of chunk denoting the next instruction to execute
    stk: LoxStack, // value stack 
}

impl VM {
    pub fn new() -> Self{
        return VM{chunk: Box::new(Chunk::new()), ip: 0, stk: LoxStack::new()};
    }
    
    pub fn interpret(&mut self, new_chunk: Chunk) -> InterpretResult { 
        self.chunk = Box::new(new_chunk);
        self.ip = 0;
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
             match DEBUG_TRACE_EXEC.get() {
                 // assumes that each chunk has starting ip of 0
                 Some(val) => {
                     if *val {
                        self.stk.print();
                         _ = self.chunk.disassemble_instruction(self.ip);
                     }
                 }
                 None => panic!("DEBUG_TRACE_EXEC is somehow empty"),
                
             }
             let instruction_number  = match self.read_byte(){
                Ok(val) => val,
                Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
             };
           // try and convert current byte to an opcode
             let cand_opcode: Option<OpCode> = num::FromPrimitive::from_u8(instruction_number);
             let opcode = match cand_opcode {
                 Some(val) => val,
                None => return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Invalid conversion  to OpCode attempted: {}", instruction_number)),       
            };
            match opcode { //finally, dispatch to the correct opcode
                OpCode::OP_RETURN => {
                    match self.stk.pop(){
                        Some(v) => {
                            v.print_value();
                            println!();
                        }
                        None => {return InterpretResult::INTERPRET_RUNTIME_ERROR("Stack is empty".to_string())}
                     }
                    return InterpretResult::INTERPRET_OK
                },
                OpCode::OP_CONSTANT => {
                    let constant_index = match self.read_byte(){
                        Ok(val) => val,
                        Err(err_msg) => return InterpretResult::INTERPRET_RUNTIME_ERROR(err_msg),
                    };
                    let constant = match self.chunk.get_constant(usize::from(constant_index)){
                        Some(val) => val,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR(format!("Couldn't access constant at address {}", constant_index))
                    };
                    self.stk.push(constant.clone());
               },
               OpCode::OP_NEGATE => {
                    let pre_val = match self.stk.pop() {
                        Some(v) => v,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR("Stack is empty".to_string()),
                    };
                    self.stk.push(value::Value{val: -pre_val.val});
               },
               OpCode::OP_ADD |
               OpCode::OP_SUBTRACT |
               OpCode::OP_DIVIDE |
               OpCode::OP_MULTIPLY => {
                    let b = match self.stk.pop(){
                        Some(v) => v,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR("Stack is empty".to_string())
                    };
                    let a = match self.stk.pop(){
                        Some(v) => v,
                        None => return InterpretResult::INTERPRET_RUNTIME_ERROR("Stack is empty".to_string())
                    };
                    match opcode {
                        OpCode::OP_ADD => self.stk.push(value::Value{val: (a.val+b.val)}),
                        OpCode::OP_SUBTRACT => self.stk.push(value::Value{val: (a.val-b.val)}),
                        OpCode::OP_MULTIPLY => self.stk.push(value::Value{val: (a.val*b.val)}),
                        OpCode::OP_DIVIDE => self.stk.push(value::Value{val: (a.val/b.val)}),
                        _ => return InterpretResult::INTERPRET_RUNTIME_ERROR("Something went horrible wrong when trying to do a binary operation".to_string())
                    }
               }
            }
        }
    }

    fn read_byte(&mut self) -> Result<u8, String> {
        let output: Result<u8,String> = match self.chunk.get_instr(self.ip){
            Some(val) => Ok(*val),
            None => Err(format!("Out of bounds access of code: {}", self.ip)),
        };
        self.ip += 1;
        return output;
    }
}
