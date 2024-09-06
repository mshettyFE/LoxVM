#![allow(non_camel_case_types)]
use std::rc::Rc;
use std::cell::RefCell;
use crate::object::LoxString;
use crate::scanner::{Token, TokenType, Scanner};
use crate::chunk::{Chunk, OpCode};
use crate::value::Value;
use crate::DEBUG_PRINT_CODE;
use crate::vm::VM;
use std::mem;

const TOTAL_RULES: usize = 40;

// the beating heart of the interpreter
// requests tokens as needed from a scanner, then converts them too bytecode
// It's basically as single pass compiler
pub struct Compiler <'a,'b>{
    compiling_chunk: &'a mut Chunk, // current chunk being executed. Could represent a function, or
                                    // a group of statements
    // the tokens under consideration
    current: Token,                 
    previous: Token,
    // error handling thingies
    hadError: bool,                 
    panicMode: bool,                // if true, denotes that the parser has no idea where it is,
                                    // and is trying it's best to get back on the right path 
    rule_table: [ParseRule<'a,'b>; TOTAL_RULES], // lookup table which maps Tokens to rules which define
                                        // prefix and infix functions, as well as the token
                                        // precedence
}

// Simple enum to denote which token an error occurred at
pub enum ErrorTokenLoc{
    PREVIOUS,
    CURRENT
}

#[derive(Copy, Clone,PartialEq, PartialOrd, FromPrimitive, Debug)]
pub enum Precedence {
 PREC_NONE,
 PREC_ASSIGNMENT, // =
 PREC_OR, // or
 PREC_AND, // and
 PREC_EQUALITY, // == !=
 PREC_COMPARISON, // < > <= >=
 PREC_TERM, // + -
 PREC_FACTOR, // * /
 PREC_UNARY, // ! -
 PREC_CALL, // . ()
 PREC_PRIMARY
}

impl Precedence{
    pub fn decrease_prec(&self) -> Result<Self,&str>{
        // converts the current variant to one that is 1 lower in precedence
        // If at the lowest precedence, throw an error
        let as_num = *self as usize;
        let total_members = mem::variant_count::<Precedence>();
        // check if as_num is the lowest precedence operator
        if as_num == (total_members-1) {
            return Err("Invalid Precedence increment");
        }

        let lower_prec = as_num+1;
        let output: Precedence =   num::FromPrimitive::from_usize(lower_prec).unwrap();
        return Ok(output);
    }
}

#[derive(Copy, Clone)]
// Some terminology: 
//      prefix refers to the tokens which *PRECEDE* some expression
//      infix refers to tokens which are *INSIDE" some expression
//      precedence refers to in what order expressions get evaluated in

// Hence, ParseRule holds two functions pointers (which can be empty) which denotes what to do if a
// prefix/infix is encounters
//
// This alone is not very useful. However, You can populate a lookup table with these ParseRules,
// and associate each rule with some token, and assign said token a precedence
// The nice thing about this is that you can add more "fixes" (like postfix) to the table
// relatively easily (after the upfront cost of updating the rules table
#[derive(Debug)]
pub struct ParseRule<'a,'b>{
    prefix: Option<fn(&mut Compiler<'a,'b>, &mut Scanner, &mut VM, bool)>,
    infix: Option<fn(&mut Compiler<'a,'b>, &mut Scanner, &mut VM, bool )>,
    precedence: Precedence,
}

impl <'a,'b> Compiler<'a,'b> where 'a: 'b{
  pub fn new (chnk: &'a mut Chunk) -> Self{
    let empty_rule: ParseRule = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
    let rules = &mut [empty_rule; TOTAL_RULES];

 // placeholder rule to copy and paste
 //    rules[TokenType:: as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 
 // God this is ugly
 rules[TokenType::TOKEN_LEFT_PAREN as usize] = ParseRule{prefix: Some(Compiler::grouping), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_RIGHT_PAREN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_LEFT_BRACE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_RIGHT_BRACE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_COMMA as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_DOT as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_MINUS as usize] = ParseRule{prefix: Some(Compiler::unary), infix: Some(Compiler::binary), precedence: Precedence::PREC_TERM};
 rules[TokenType::TOKEN_PLUS as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence: Precedence::PREC_TERM};
 rules[TokenType::TOKEN_SEMICOLON as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_SLASH as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence: Precedence::PREC_FACTOR};
 rules[TokenType::TOKEN_STAR as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence: Precedence::PREC_FACTOR};
 rules[TokenType::TOKEN_BANG as usize] = ParseRule{prefix: Some(Compiler::unary), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_BANG_EQUAL as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence: Precedence::PREC_EQUALITY};
 rules[TokenType::TOKEN_EQUAL as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_EQUAL_EQUAL as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence: Precedence::PREC_EQUALITY};
 rules[TokenType::TOKEN_GREATER as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence: Precedence::PREC_COMPARISON};
 rules[TokenType::TOKEN_GREATER_EQUAL as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence:  Precedence::PREC_COMPARISON};
 rules[TokenType::TOKEN_LESS as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence: Precedence::PREC_COMPARISON};
 rules[TokenType::TOKEN_LESS_EQUAL as usize] = ParseRule{prefix: None, infix: Some(Compiler::binary), precedence: Precedence::PREC_COMPARISON};
 rules[TokenType::TOKEN_IDENTIFIER as usize] = ParseRule{prefix: Some(Compiler::variable), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_STRING as usize] = ParseRule{prefix: Some(Compiler::string), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_NUMBER as usize] = ParseRule{prefix: Some(Compiler::number), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_AND as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_CLASS as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_ELSE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FALSE as usize] = ParseRule{prefix: Some(Compiler::literal), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FOR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FUN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_IF as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_NIL as usize] = ParseRule{prefix: Some(Compiler::literal), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_OR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_PRINT as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_RETURN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_SUPER as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_THIS as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_TRUE as usize] = ParseRule{prefix: Some(Compiler::literal), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_VAR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_WHILE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_ERROR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_EOF as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
    
 Compiler{compiling_chunk: chnk, 
        previous: Token {ttype: TokenType::TOKEN_ERROR, start: "".to_string(), line: 0 },
        current: Token { ttype: TokenType::TOKEN_ERROR, start: "".to_string(), line: 0 },
        hadError: false,
        panicMode: false,
        rule_table: *rules,
    }
  }
    pub fn compile(&mut self, scanner: &mut Scanner, vm: &mut VM) -> bool{
        // interface of parser. Give it a scanner, and it will chew through the tokens, spitting
        // out byte code
        // returns false if an error has occurred
        self.advance(scanner, vm);
        loop {
           if self.Match(scanner,vm, TokenType::TOKEN_EOF){
                break;
           } 
           self.declaration(scanner, vm);
        }
        // last token needs to be EOF, otherwise, we have problems
        self.consume(TokenType::TOKEN_EOF, "Expected end of expression.".to_string(), scanner, vm);
        self.endCompiler();
        return !self.hadError;
    }

    fn advance(&mut self, scanner: &mut Scanner, _vm: &mut VM){
        // save current token (to be able to later access the lexeme), and then ask the scanner for
        // a new token
        //
        // If an error is encountered, then keep advancing until they stop occurring
        // Panic mode flag should be set at some point so that you don't fill the log with
        // pointless error messages while panicking
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

    fn Match(&mut self, scanner: &mut Scanner, vm: &mut VM, wanted_token_type: TokenType) -> bool{
        if self.current.ttype != wanted_token_type { return false;}
        self.advance(scanner, vm);
        return true;
    }
    
    fn consume(&mut self, wanted_token: TokenType, err_msg: String, scanner: &mut Scanner, vm: &mut VM){
        if self.current.ttype == wanted_token {
            self.advance(scanner, vm);
            return
        }
        self.errorAt(err_msg, ErrorTokenLoc::CURRENT);
    }


    
    fn endCompiler(&mut self){
        // tops of chunk with return, and then disassembles if prompted
        self.emitReturn();
        if *DEBUG_PRINT_CODE.get().unwrap(){
            if !self.hadError{
                let _ = self.currentChunk().disassemble("code".to_string());
            }
        }
    }


// grammar production functions
// see https://craftinginterpreters.com/image/compiling-expressions/connections.png

    fn declaration(&mut self, scanner: &mut Scanner, vm: &mut VM){
        if self.Match(scanner, vm, TokenType::TOKEN_VAR){
            self.varDeclaration(scanner, vm);
        } else {
            self.statement(scanner, vm); 
        }
        if self.panicMode {
            self.synchronize(scanner, vm);
        }
    }

    fn varDeclaration(&mut self, scanner: &mut Scanner, vm: &mut VM){
        let global = self.
            parseVariable(
                "Expect variable name".to_string(), scanner, vm);
        if self.Match(scanner, vm, TokenType::TOKEN_EQUAL) {
            self.expression(scanner, vm);
        } else {
            self.emitByte(OpCode::OP_NIL as u8);
        }
        self.consume(TokenType::TOKEN_SEMICOLON, "Expect ';' after variable declaration.".to_string(), scanner, vm);
        self.defineVariable(global);
    }
    
    fn statement(&mut self, scanner: &mut Scanner, vm: &mut VM){
        if self.Match(scanner, vm, TokenType::TOKEN_PRINT){
            self.printStatement(scanner, vm); 
        } else {
            self.expressionStatement(scanner, vm);
        }
    }

    fn printStatement(&mut self, scanner: &mut Scanner, vm: &mut VM){
        self.expression(scanner, vm);
        self.consume(TokenType::TOKEN_SEMICOLON, "Expect ; after value.".to_string(), scanner, vm);
        self.emitByte(OpCode::OP_PRINT as u8);

    }

    fn expressionStatement(&mut self, scanner: &mut Scanner, vm: &mut VM){
        self.expression(scanner, vm);
        self.consume(TokenType::TOKEN_SEMICOLON, "Expect ; after value.".to_string(), scanner, vm);
        self.emitByte(OpCode::OP_POP as u8);
    }

    fn expression(&mut self, scanner: &mut Scanner, vm: &mut VM) {
        // interpret an expression
        self.parsePrecedence(Precedence::PREC_ASSIGNMENT, scanner, vm);
    }

    fn grouping(&mut self, scanner: &mut Scanner, vm: &mut VM, _canAssign: bool){
        // do a bunch of expression wrapped in parentheses
        self.expression(scanner, vm);
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expected ')' after expression".to_string(), scanner, vm);
    } 

    fn unary(&mut self, scanner: &mut Scanner, vm: &mut VM, _canAssign: bool){
        // once you you hit a unary, you need to process the the following expression while
        // respecting precedence
        let operatorType = self.previous.ttype.clone();
        self.parsePrecedence(Precedence::PREC_UNARY, scanner, vm);
        // after the fact, you apply the unary
        match operatorType {
            TokenType::TOKEN_BANG => {self.emitByte(OpCode::OP_NOT as u8); () },
            TokenType::TOKEN_MINUS => {self.emitByte(OpCode::OP_NEGATE as u8); ()},
            _ => (),
        }
    }
    
    fn binary(&mut self, scanner: &mut Scanner, vm: &mut VM, _canAssign: bool){
        let operatorType = self.previous.ttype.clone();
        let rule = self.getRule(operatorType);
        self.parsePrecedence(rule.unwrap().precedence.decrease_prec().unwrap(), scanner, vm);
        match operatorType {
            TokenType::TOKEN_BANG_EQUAL => {self.emitTwoBytes(OpCode::OP_EQUAL as u8, OpCode::OP_NOT as u8); ()},
            TokenType::TOKEN_EQUAL_EQUAL => {self.emitByte(OpCode::OP_EQUAL as u8); ()},
            TokenType::TOKEN_GREATER => {self.emitByte(OpCode::OP_GREATER as u8); ()},
            TokenType::TOKEN_GREATER_EQUAL => {self.emitTwoBytes(OpCode::OP_LESS as u8, OpCode::OP_NOT as u8); ()},
            TokenType::TOKEN_LESS => {self.emitByte(OpCode::OP_LESS as u8); ()},
            TokenType::TOKEN_LESS_EQUAL => {self.emitTwoBytes(OpCode::OP_GREATER as u8, OpCode::OP_NOT as u8); ()},
            TokenType::TOKEN_PLUS => {self.emitByte(OpCode::OP_ADD as u8); ()},
            TokenType::TOKEN_MINUS => {self.emitByte(OpCode::OP_SUBTRACT as u8); ()},
            TokenType::TOKEN_STAR => {self.emitByte(OpCode::OP_MULTIPLY as u8); ()},
            TokenType::TOKEN_SLASH => {self.emitByte(OpCode::OP_DIVIDE as u8); ()},
            _ => (),
        }
    }
    
    fn number(&mut self, _scanner: &mut Scanner, _vm: &mut VM, _canAssign: bool){
        // cast the previous token's string into a double and add said constant to chunk
        let value: f64 = self.previous.start.parse().unwrap(); 
        self.emitConstant(Value::VAL_NUMBER(value));
    }

    fn literal(&mut self, _scanner: &mut Scanner, _vm: &mut VM, _canAssign: bool){
        match self.previous.ttype {
            TokenType::TOKEN_FALSE => {self.emitByte(OpCode::OP_FALSE as u8); ()},
            TokenType::TOKEN_TRUE => {self.emitByte(OpCode::OP_TRUE as u8); ()},
            TokenType::TOKEN_NIL => {self.emitByte(OpCode::OP_NIL as u8); ()},
            _ => {()}
        } 
    }

    fn string(&mut self, _scanner: &mut Scanner, _vm: &mut VM, _canAssign: bool){
        let quoted = self.previous.start.clone();
        let len = quoted.len();
        // trim of quotes
        let unquoted = &quoted[1..len-1];
        let new_string = LoxString::new(unquoted.to_string());
        let data =  Value::VAL_OBJ(Rc::new( RefCell::new(new_string)));
        self.emitConstant(data); 
    }

    fn variable(&mut self, scanner: &mut Scanner, vm: &mut VM, canAssign: bool){
        self.namedVariable(self.previous.clone(), scanner, vm, canAssign);
    }

    fn namedVariable(&mut  self, name: Token, scanner: &mut Scanner, vm: &mut VM, canAssign: bool){
        let arg = self.identifierConstant(name);
        if self.Match(scanner, vm, TokenType::TOKEN_EQUAL) && canAssign{
            self.expression(scanner, vm);
            self.emitTwoBytes(OpCode::OP_SET_GLOBAL as u8, arg);
        }
        self.emitTwoBytes(OpCode::OP_GET_GLOBAL as u8, arg);
    }
    
// the function that handles precedence and allows recursion to occur
    fn parsePrecedence(&mut self, init_precedence: Precedence, scanner: &mut Scanner, vm: &mut VM){
        self.advance(scanner, vm);
        let canAssign: bool;
        // Check if a prefix operation needs to be performed
        let get_prefix: Option<ParseRule> = self.getRule(self.previous.ttype.clone());
        match get_prefix {
            Some(val) => {
                match val.prefix{
                    // after unwrapping everything, execute the prefix function
                    Some(prefix_func) => {
                        canAssign = init_precedence <= Precedence::PREC_ASSIGNMENT;
                        prefix_func(self, scanner, vm, canAssign);
                    },
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

        // perform any infix operations
        loop {
            let is_cur_rule = self.getRule(self.current.ttype);
            let cur_rule = match is_cur_rule {
                Some(val) => val,
                None => {
                    self.errorAt("Out of bounds of rule table on infix".to_string(), ErrorTokenLoc::PREVIOUS);
                    return ;
                },
            };

            // you stop performing infix operations if the current precedence falls below the
            // initial one 
            if init_precedence as u8 > cur_rule.precedence as u8 {break;}
            // otherwise, perform the infix operation and keep going
            // update current and previous tokens
            self.advance(scanner, vm);
            // now need to read previous rule (ie. the binary op) and perform the infix
            let is_prev_rule = self.getRule(self.previous.ttype);
            let prev_rule = match is_prev_rule {
                Some(val) => val,
                None => {
                    self.errorAt("Out of bounds of rule table on infix".to_string(), ErrorTokenLoc::PREVIOUS);
                    return ;
                },
            };

            match prev_rule.infix {
                Some(function) => function(self, scanner, vm, canAssign),
                None => {
                    self.errorAt("Invalid infix rule".to_string(), ErrorTokenLoc::PREVIOUS);
                    return;
                }
            }
            if canAssign && self.Match(scanner, vm, TokenType::TOKEN_EQUAL){
                self.errorAt("Invalid assignment target".to_string(), ErrorTokenLoc::PREVIOUS);
                return;
            }
        }
    }

    fn parseVariable(&mut self, err_msg: String, scanner: &mut Scanner, vm: &mut VM) -> u8{
        self.consume(TokenType::TOKEN_IDENTIFIER, err_msg, scanner, vm); 
        return self.identifierConstant(self.previous.clone())
    }

    fn defineVariable(&mut self, global: u8){
        self.emitTwoBytes(OpCode::OP_DEFINE_GLOBAL as u8, global);
    }

    fn identifierConstant(&mut self, name: Token) -> u8{
        let str = LoxString::new(name.start);
        let val = Value::VAL_OBJ(Rc::new( RefCell::new(str)));
        self.makeConstant(val)
    }

    fn getRule(&mut self, ttype: TokenType)  -> Option<ParseRule<'a,'b>>{
        // return the corresponding parseRule for a given token from the lookup table
        let index = ttype as usize;
        match self.rule_table.get(index){
            Some(val) => {return Some(*val)},
            None => {println!("Nothing");None},
        }
    }

    fn emitByte(&mut self, new_byte: u8){
        // add a byte to the current chunk
        let line = self.previous.line;
        self.currentChunk().write_chunk(new_byte, line);
    }

    fn emitTwoBytes(&mut self, byte1: u8, byte2: u8){
        // What do you think it does?
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn emitConstant(&mut self, value: Value){
        // adds the opcode and the constant index
        let cnst = self.makeConstant(value);
        self.emitTwoBytes(OpCode::OP_CONSTANT as u8, cnst);
    }

    fn makeConstant(&mut self, new_val: Value) -> u8{
        let cnst_index = self.currentChunk().add_constant(new_val); 
        // arbitrarily cap number of constants at u8::MAX because you have to draw the line
        // somewhere
        if cnst_index > std::u8::MAX as usize {
            self.errorAt("Too many constants in one chunk".to_string(), ErrorTokenLoc::PREVIOUS);
            return 0;
        }
        return cnst_index as u8;
    }

    fn emitReturn(&mut self) {
        self.emitByte(OpCode::OP_RETURN as u8);
    }
    
    fn currentChunk(&mut self) -> &mut Chunk{
        self.compiling_chunk
    }

    fn errorAt(&mut self, msg: String, which_token: ErrorTokenLoc){
        // spits out the an error message if not panicking to try and reorient itself
        if self.panicMode {return}
        let token = match which_token{
            ErrorTokenLoc::PREVIOUS => &self.previous,
            ErrorTokenLoc::CURRENT => &self.current,
        };

        // turn on panicMode so that compiler doesn't spit out errors while it reorients itself
        self.panicMode = true;
        
        eprint!("[line {}] Error: ", token.line);
        match token.ttype {
            TokenType::TOKEN_EOF =>{
                eprint!(" at end");
                ()
            }
            TokenType::TOKEN_ERROR =>{
                () 
            }
            _ => {
                eprint!("at {} :", token.start)
            }
        }

        eprintln!("{}", msg);
        self.hadError = true;
    }

    fn synchronize(&mut self, scanner: &mut Scanner, vm: &mut VM){
        self.panicMode = false;
        loop {
            if self.current.ttype == TokenType::TOKEN_EOF {
                break;
            }
            match self.current.ttype {
                TokenType::TOKEN_CLASS => {return },
                TokenType::TOKEN_FUN => {return },
                TokenType::TOKEN_VAR => {return },
                TokenType::TOKEN_FOR => {return },
                TokenType::TOKEN_IF => {return },
                TokenType::TOKEN_WHILE => {return },
                TokenType::TOKEN_PRINT => {return },
                TokenType::TOKEN_RETURN => {return },
                // do nothing for other tokens and just advance
                _ => {
                    
                }
            }
            self.advance(scanner, vm);
        }
    }
}

pub fn isFalsey(value: Value)-> bool{
    // Ruby style truthy-ness
    //      nil and false are   false
    //      everything else is true
    match value {
        Value::VAL_NIL => {return true},
        Value::VAL_BOOL(val) => {return !val},
        _ => {return  false}        
    }
}

