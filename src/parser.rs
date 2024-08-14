#![allow(non_camel_case_types)]
use crate::{chunk, scanner::*};
use crate::chunk::{Chunk, Disassemble, OpCode};
use crate::value::Value;
use crate::DEBUG_PRINT_CODE;

pub struct Parser <'a,'b>{
    compiling_chunk: &'a mut Chunk,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
    rule_table: [ParseRule<'a,'b>; 40],
}

pub enum ErrorTokenLoc{
    PREVIOUS,
    CURRENT
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Precedence {
 PREC_NONE = 0,
 PREC_ASSIGNMENT=1, // =
 PREC_OR=2, // or
 PREC_AND=3, // and
 PREC_EQUALITY=4, // == !=
 PREC_COMPARISON=5, // < > <= >=
 PREC_TERM=6, // + -
 PREC_FACTOR=7, // * /
 PREC_UNARY=8, // ! -
 PREC_CALL=9, // . ()
 PREC_PRIMARY=10
}


impl Precedence{
    pub fn shuffle(&self) -> Result<Self,&str>{
        match *self {
            Precedence::PREC_ASSIGNMENT => return Ok(Precedence::PREC_OR),
            Precedence::PREC_OR => return Ok(Precedence::PREC_AND),
            Precedence::PREC_AND => return Ok(Precedence::PREC_EQUALITY),
            Precedence::PREC_EQUALITY => return Ok(Precedence::PREC_COMPARISON),
            Precedence::PREC_COMPARISON => return Ok(Precedence::PREC_TERM),
            Precedence::PREC_TERM => return Ok(Precedence::PREC_FACTOR),
            Precedence::PREC_FACTOR => return Ok(Precedence::PREC_UNARY),
            Precedence::PREC_UNARY => return Ok(Precedence::PREC_CALL),
            Precedence::PREC_CALL => return Ok(Precedence::PREC_PRIMARY),
            _ => return Err("Invalid Precedence increment"),
        }
    }
}

#[derive(Copy, Clone)]
pub struct ParseRule<'a,'b>{
    prefix: Option<fn(&mut Parser<'a,'b>, &mut Scanner)>,
    infix: Option<fn(&mut Parser<'a,'b>, &mut Scanner)>,
    precedence: Precedence,
}

impl <'a,'b> Parser<'a,'b> where 'a: 'b{
  pub fn new (chnk: &'a mut Chunk) -> Self{
    let empty_rule: ParseRule = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
    let rules = &mut [empty_rule; 40];

 // God this is ugly
 //    rules[TokenType:: as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_LEFT_PAREN as usize] = ParseRule{prefix: Some(Parser::grouping), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_RIGHT_PAREN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_LEFT_BRACE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_RIGHT_BRACE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_COMMA as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_DOT as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_MINUS as usize] = ParseRule{prefix: Some(Parser::unary), infix: Some(Parser::binary), precedence: Precedence::PREC_TERM};
 rules[TokenType::TOKEN_PLUS as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence: Precedence::PREC_TERM};
 rules[TokenType::TOKEN_SEMICOLON as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_SLASH as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence: Precedence::PREC_FACTOR};
 rules[TokenType::TOKEN_STAR as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence: Precedence::PREC_FACTOR};
 rules[TokenType::TOKEN_BANG as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_BANG_EQUAL as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_EQUAL as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_EQUAL_EQUAL as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_GREATER as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_GREATER_EQUAL as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_LESS as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_LESS_EQUAL as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_IDENTIFIER as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_STRING as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_NUMBER as usize] = ParseRule{prefix: Some(Parser::number), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_AND as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_CLASS as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_ELSE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FALSE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FOR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FUN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_IF as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_NIL as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_OR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_PRINT as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_RETURN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_SUPER as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_THIS as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_TRUE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_VAR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_WHILE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_ERROR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_EOF as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
    
 Parser{compiling_chunk: chnk, 
        previous: Token {ttype: TokenType::TOKEN_ERROR, start: "".to_string(), line: 0 },
        current: Token { ttype: TokenType::TOKEN_ERROR, start: "".to_string(), line: 0 },
        hadError: false,
        panicMode: false,
        rule_table: *rules,
    }
  }
    pub fn compile(&mut self, scanner: &mut Scanner) -> bool{
        self.advance(scanner);
        self.expression(scanner);
        self.consume(TokenType::TOKEN_EOF, "Expected end of expression.".to_string(), scanner);
        self.endCompiler();
        return !self.hadError;
    }


    pub fn advance(&mut self, scanner: &mut Scanner){
        self.previous =  self.current.clone();
        loop{
            self.current = scanner.scanToken();
            match self.current.ttype {
                TokenType::TOKEN_ERROR => (),
                _ => break,
            }

            self.errorAt(self.current.start.clone(), ErrorTokenLoc::CURRENT);
        }
    }

    fn expression(&mut self, scanner: &mut Scanner) {
        self.parsePrecedence(Precedence::PREC_ASSIGNMENT, scanner);
    }

    fn grouping(&mut self, scanner: &mut Scanner){
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expected ')' after expression".to_string(), scanner);
    } 

    fn unary(&mut self, scanner: &mut Scanner){
        let operatorType = self.previous.ttype.clone();
        self.parsePrecedence(Precedence::PREC_UNARY, scanner);
        match operatorType {
            TokenType::TOKEN_MINUS => {self.emitByte(OpCode::OP_NEGATE as u8); ()},
            _ => (),
        }
    }

    fn parsePrecedence(&mut self, init_precedence: Precedence, scanner: &mut Scanner){
        self.advance(scanner);
        let get_prefix: Option<ParseRule> = self.getRule(self.previous.ttype.clone());
        match get_prefix {
            Some(val) => {
                match val.prefix{
                    Some(prefix_func) => prefix_func(self, scanner),
                    None => {
                        self.errorAt("Expect expression".to_string(), ErrorTokenLoc::PREVIOUS);
                        return ;
                    }
                }
            }
            None => {
                self.errorAt("Out of bounds of rule table".to_string(), ErrorTokenLoc::PREVIOUS);
                return ;
            }
        }

        loop {
        // check for condition first
            let is_cur_rule = self.getRule(self.current.ttype);
            let cur_rule = match is_cur_rule {
                Some(val) => val,
                None => {
                    self.errorAt("Out of bounds of rule table on infix".to_string(), ErrorTokenLoc::PREVIOUS);
                    return ;
                },
            };

            let is_prev_rule = self.getRule(self.previous.ttype);
            let prev_rule = match is_prev_rule {
                Some(val) => val,
                None => {
                    self.errorAt("Out of bounds of rule table on infix".to_string(), ErrorTokenLoc::PREVIOUS);
                    return ;
                },
            };


            if init_precedence as u8 > cur_rule.precedence as u8 {break;}
            self.advance(scanner);
            prev_rule.infix.expect("Invalid infix rule")(self, scanner);
            
        }


    }

    fn getRule(&mut self, ttype: TokenType)  -> Option<ParseRule<'a,'b>>{
        let index = ttype as usize;
        match self.rule_table.get(index){
            Some(val) => return Some(*val),
            None => None,
        }
    }

    fn number(&mut self, _scanner: &mut Scanner){
        let value: f64 = self.previous.start.parse().unwrap();  
        self.emitConstant(value);
    }

    fn consume(&mut self, wanted_token: TokenType, err_msg: String, scanner: &mut Scanner){
        if self.current.ttype == wanted_token {
            self.advance(scanner);
            return
        }
        self.errorAt(err_msg, ErrorTokenLoc::CURRENT);
    }

    fn emitByte(&mut self, new_byte: u8){
        let line = self.previous.line;
        self.currentChunk().write_chunk(new_byte, line);
    }

    fn emitBytes(&mut self, byte1: u8, byte2: u8){
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn emitConstant(&mut self, value: f64){
        let cnst = self.makeConstant(value);
        self.emitBytes(chunk::OpCode::OP_CONSTANT as u8, cnst);
    }

    fn makeConstant(&mut self, new_val: f64) -> u8{
        let cnst = self.currentChunk().add_constant(Value{val: new_val});  
        if cnst > std::u8::MAX as usize {
            self.errorAt("Too many constants in one chunk".to_string(), ErrorTokenLoc::PREVIOUS);
            return 0;
        }
        return cnst as u8;
    }


    fn endCompiler(&mut self){
        self.emitReturn();
        if *DEBUG_PRINT_CODE.get().unwrap(){
            if !self.hadError{
                let _ = self.currentChunk().disassemble("code".to_string());
            }
        }
    }

    fn binary(&mut self, scanner: &mut Scanner){
        let operatorType = self.previous.ttype.clone();

        let rule = self.getRule(operatorType);
        self.parsePrecedence(rule.unwrap().precedence.shuffle().unwrap(), scanner);
        match operatorType {
            TokenType::TOKEN_PLUS => {self.emitByte(OpCode::OP_ADD as u8); ()},
            TokenType::TOKEN_MINUS => {self.emitByte(OpCode::OP_SUBTRACT as u8); ()},
            TokenType::TOKEN_STAR => {self.emitByte(OpCode::OP_MULTIPLY as u8); ()},
            TokenType::TOKEN_SLASH => {self.emitByte(OpCode::OP_DIVIDE as u8); ()},
            _ => (),
        }
    }

    fn emitReturn(&mut self) {
        self.emitByte(chunk::OpCode::OP_RETURN as u8);
    }
    
    fn currentChunk(&mut self) -> &mut Chunk{
        self.compiling_chunk
    }



    fn errorAt(&mut self, msg: String, which_token: ErrorTokenLoc){
        if self.panicMode {return}
        let token = match which_token{
            ErrorTokenLoc::PREVIOUS => &self.previous,
            ErrorTokenLoc::CURRENT => &self.current,
        };

        self.panicMode = true;
        
        eprint!("[line {}] Error", token.line);
        match token.ttype {
            TokenType::TOKEN_EOF =>{
                eprint!(" at end");
                ()
            }
            TokenType::TOKEN_ERROR =>{
                () 
            }
            _ => {
                eprint!("at {}", token.start)
            }
        }

        eprintln!("{}", msg);
        self.hadError = true;
    }
}
