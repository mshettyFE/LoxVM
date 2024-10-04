#![allow(non_camel_case_types)]
use std::rc::Rc;
use std::cell::RefCell;
use crate::compiler_stack::CompilerStack;
use crate::object::{LoxFunction, LoxString};
use crate::scanner::{Token, TokenType, Scanner};
use crate::chunk::{Chunk, OpCode};
use crate::value::Value;
use crate::DEBUG_PRINT_CODE;
use std::mem;

// size of ParseRules lookup table
const TOTAL_RULES: usize = 40;

pub enum FunctionType {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
}


pub struct Compiler {
    pub function: Option<LoxFunction>,
    ftype: FunctionType,
    locals: Vec<Locals>, // a stack of local variables that are accessible in the current scope
    localCount: u64, // a count of the number of variables accessible in the current scope
    scopeDepth: u64 // the current scope
}

impl Compiler {
    pub fn new(new_ftype: FunctionType) -> Self{
        Compiler{
            function: Some(LoxFunction::new(0, None)),
            ftype: new_ftype,
            locals: Vec::new(),
            localCount: 0,
            scopeDepth: 0
        }
    }

    pub fn beginScope(&mut self){
        self.scopeDepth += 1;
    }
    
    pub fn endScope(&mut self){
        self.scopeDepth -= 1;
    }
   
    fn markInitialized(& mut self) {
        if (self.scopeDepth == 0 ) {return}
        // utilized to prevent self-initialization (ie. var a = a; type of thing).
        // Use option as a sentinel value
        self.locals.get_mut((self.localCount) as usize -1).unwrap().depth = 
            Some(self.scopeDepth);
    }
}

pub struct Locals {
    pub name: Token, // name of variable
    pub depth: Option<u64>, // scope of local
}

impl Locals {
    pub fn new(new_name: Token, new_depth: Option<u64>) -> Self{
        Locals{name: new_name, depth: new_depth}
    }
}

// the beating heart of the interpreter
// requests tokens as needed from a scanner, then converts them too bytecode
// It's basically as single pass compiler
pub struct Parser{
    compilerStack: CompilerStack, // current chunk being executed. Could represent a function, or
                                    // a group of statements
    // the tokens under consideration
    current: Token,                 
    previous: Token,
    // error handling thingies
    hadError: bool,                 
    panicMode: bool,                // if true, denotes that the parser has no idea where it is,
                                    // and is trying it's best to get back on the right path 
    rule_table: [ParseRule; TOTAL_RULES], // lookup table which maps Tokens to rules which define
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
pub struct ParseRule{
    prefix: Option<fn(&mut Parser, &mut Scanner, bool)>,
    infix: Option<fn(&mut Parser, &mut Scanner, bool )>,
    precedence: Precedence,
}

impl Parser{
  pub fn new () -> Self{
    let empty_rule: ParseRule = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
    let rules = &mut [empty_rule; TOTAL_RULES];

 // placeholder rule to copy and paste
 //    rules[TokenType:: as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 
 // God this is ugly
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
 rules[TokenType::TOKEN_BANG as usize] = ParseRule{prefix: Some(Parser::unary), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_BANG_EQUAL as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence: Precedence::PREC_EQUALITY};
 rules[TokenType::TOKEN_EQUAL as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_EQUAL_EQUAL as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence: Precedence::PREC_EQUALITY};
 rules[TokenType::TOKEN_GREATER as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence: Precedence::PREC_COMPARISON};
 rules[TokenType::TOKEN_GREATER_EQUAL as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence:  Precedence::PREC_COMPARISON};
 rules[TokenType::TOKEN_LESS as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence: Precedence::PREC_COMPARISON};
 rules[TokenType::TOKEN_LESS_EQUAL as usize] = ParseRule{prefix: None, infix: Some(Parser::binary), precedence: Precedence::PREC_COMPARISON};
 rules[TokenType::TOKEN_IDENTIFIER as usize] = ParseRule{prefix: Some(Parser::variable), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_STRING as usize] = ParseRule{prefix: Some(Parser::string), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_NUMBER as usize] = ParseRule{prefix: Some(Parser::number), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_AND as usize] = ParseRule{prefix: None, infix: Some(Parser::and), precedence: Precedence::PREC_AND};
 rules[TokenType::TOKEN_CLASS as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_ELSE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FALSE as usize] = ParseRule{prefix: Some(Parser::literal), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FOR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_FUN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_IF as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_NIL as usize] = ParseRule{prefix: Some(Parser::literal), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_OR as usize] = ParseRule{prefix: None, infix: Some(Parser::or), precedence: Precedence::PREC_OR};
 rules[TokenType::TOKEN_PRINT as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_RETURN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_SUPER as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_THIS as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_TRUE as usize] = ParseRule{prefix: Some(Parser::literal), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_VAR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_WHILE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_ERROR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_EOF as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
    
 Parser{compilerStack: CompilerStack::new(), 
        previous: Token {ttype: TokenType::TOKEN_ERROR, start: "".to_string(), line: 0 },
        current: Token { ttype: TokenType::TOKEN_ERROR, start: "".to_string(), line: 0 },
        hadError: false,
        panicMode: false,
        rule_table: *rules,
    }
  }
    pub fn compile(&mut self, scanner: &mut Scanner) -> Option<Compiler>{
        // interface of parser. Give it a scanner, and it will chew through the tokens, spitting
        // out byte code
        // returns false if an error has occurred
        self.advance(scanner);
        loop {
           if self.Match(scanner, TokenType::TOKEN_EOF){
                break;
           } 
           self.declaration(scanner);
        }
        // last token needs to be EOF, otherwise, we have problems
        self.consume(TokenType::TOKEN_EOF, "Expected end of expression.".to_string(), scanner);
        let out = self.endParser();
        match self.hadError {
            true => None,
            false => out
        }
    }

    fn advance(&mut self, scanner: &mut Scanner){
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

    fn Match(&mut self, scanner: &mut Scanner, wanted_token_type: TokenType) -> bool{
        // check if current token matches what you want. If so, advance in stream
        if self.current.ttype != wanted_token_type { return false;}
        self.advance(scanner);
        return true;
    }
   
    fn consume(&mut self, wanted_token: TokenType, err_msg: String, scanner: &mut Scanner, ){
        // check if current matches expected. If not, throw error message
        if self.current.ttype == wanted_token {
            self.advance(scanner);
            return
        }
        self.errorAt(err_msg, ErrorTokenLoc::CURRENT);
    }
 
    fn endParser(&mut self) -> Option<Compiler>{
        // tops off chunk with return, and then disassembles if prompted
        self.emitReturn();
        let function = &self.compilerStack.peek_mut().unwrap().function;
        if *DEBUG_PRINT_CODE.get().unwrap(){
            if !self.hadError{
                let msg = match &function {
                    Some(fnc) => match &fnc.name {
                        Some(v) => v.val.clone(),
                        None => "<script>".to_string(),
                    }
                    None => panic!("Couldn't load function")
                };
                let _ = self.currentChunk().disassemble(msg);
            }
        }
        // move up to new compiler
        let out = self.compilerStack.pop();
       return out;
    }


// grammar production functions
// see https://craftinginterpreters.com/image/compiling-expressions/connections.png
// These are effectively translating the BNF notation of Lox into code

    fn declaration(&mut self, scanner: &mut Scanner, ){
        if self.Match(scanner, TokenType::TOKEN_FUN){
            self.funDeclaration(scanner);
        }
        else if self.Match(scanner, TokenType::TOKEN_VAR){
            self.varDeclaration(scanner);
        } else {
            self.statement(scanner); 
        }
        if self.panicMode {
            self.synchronize(scanner);
        }
    }

    fn varDeclaration(&mut self, scanner: &mut Scanner,  ){
        let global = self.parseVariable("Expect variable name".to_string(), scanner);
        if self.Match(scanner, TokenType::TOKEN_EQUAL) {
            self.expression(scanner);
        } else {
            self.emitByte(OpCode::OP_NIL as u8);
        }
        self.consume(TokenType::TOKEN_SEMICOLON, "Expect ';' after variable declaration.".to_string(), scanner);
        self.defineVariable(global);
    }

    fn funDeclaration(&mut self, scanner: &mut Scanner){
        let global = self.parseVariable("Expect function name.".to_string(), scanner);
        self.compilerStack.peek_mut().unwrap().markInitialized();
        self.function(FunctionType::TYPE_FUNCTION, scanner);
        self.defineVariable(global);
    }
    
    fn statement(&mut self, scanner: &mut Scanner ){
        if self.Match(scanner, TokenType::TOKEN_PRINT){
            self.printStatement(scanner); 
        } else if self.Match(scanner, TokenType::TOKEN_FOR){
            self.forStatement(scanner);
        }
        else if self.Match(scanner, TokenType::TOKEN_IF){
            self.ifStatement(scanner );
        } else if self.Match(scanner, TokenType::TOKEN_WHILE){
            self.whileStatement(scanner);
        } 
        else if self.Match(scanner, TokenType::TOKEN_LEFT_BRACE){
            self.compilerStack.peek_mut().unwrap().beginScope();
            self.block(scanner);
            self.compilerStack.peek_mut().unwrap().endScope();
            loop {
                if self.compilerStack.peek_mut().unwrap().localCount == 0 {
                    break;
                }
                let cur_comp = self.compilerStack.peek_mut().unwrap();
                let var_depth = cur_comp.locals.get( (cur_comp.localCount-1) as usize ).unwrap().depth;
                if var_depth.unwrap() > self.compilerStack.peek_mut().unwrap().scopeDepth {
                    self.emitByte(OpCode::OP_POP as u8);
                    self.compilerStack.peek_mut().unwrap().localCount -= 1;
                    continue;
                }
                break;
            }
        }
        else {
            self.expressionStatement(scanner);
        }
    }

    fn printStatement(&mut self, scanner: &mut Scanner){
        self.expression(scanner);
        self.consume(TokenType::TOKEN_SEMICOLON, "Expect ; after value.".to_string(), scanner);
        self.emitByte(OpCode::OP_PRINT as u8);
    }

    fn expressionStatement(&mut self, scanner: &mut Scanner ){
        self.expression(scanner);
        self.consume(TokenType::TOKEN_SEMICOLON, "Expect ; after value.".to_string(), scanner);
        self.emitByte(OpCode::OP_POP as u8);
    }

    fn forStatement(&mut self, scanner: &mut Scanner){
// Out of bounds problem here        
        self.compilerStack.peek_mut().unwrap().beginScope(); 
        self.consume(TokenType::TOKEN_LEFT_PAREN, "Expect '(' after 'for'.".to_string(), scanner);
        if self.Match(scanner, TokenType::TOKEN_SEMICOLON){}
        else if self.Match(scanner, TokenType::TOKEN_VAR){
            self.varDeclaration(scanner);
        } else {
            self.expressionStatement(scanner);
        }
        

        let mut loopStart = self.currentChunk().get_count();

        let exitJump: Option<usize>;
        if !self.Match(scanner, TokenType::TOKEN_SEMICOLON) {
            self.expression(scanner);
            self.consume(TokenType::TOKEN_SEMICOLON, "Expect ';' after condition.".to_string(), scanner);
            
            exitJump =  Some(self.emitJump(OpCode::OP_JUMP_IF_FALSE));
            self.emitByte(OpCode::OP_POP as u8);
        } else{
            exitJump = None;
        }
        
        if  !self.Match(scanner, TokenType::TOKEN_RIGHT_PAREN) {
            let bodyJump = self.emitJump(OpCode::OP_JUMP);
            let incrementStart = self.currentChunk().get_count();
            self.expression(scanner);
            self.emitByte(OpCode::OP_POP as u8);
            self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect ')' after 'for'.".to_string(), scanner);

            self.emitLoop(loopStart);
            loopStart = incrementStart;

            self.patchJump(bodyJump);
        }

        self.statement(scanner);

        self.emitLoop(loopStart);
        match exitJump {
            Some(offset) => {self.patchJump(offset); self.emitByte(OpCode::OP_POP as u8); ()}
            None => ()
        }

        self.compilerStack.peek_mut().unwrap().endScope();

    }

    fn ifStatement(&mut self, scanner: &mut Scanner){
        self.consume(TokenType::TOKEN_LEFT_PAREN, "Expect '(' after 'if'.".to_string(), scanner);
        self.expression(scanner);
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect '(' after condition.".to_string(), scanner);
        let thenJump: usize = self.emitJump(OpCode::OP_JUMP_IF_FALSE); 
        self.emitByte(OpCode::OP_POP as u8);
        self.statement(scanner);
        
        let elseJump: usize = self.emitJump(OpCode::OP_JUMP);
        self.patchJump(thenJump);
        self.emitByte(OpCode::OP_POP as u8);

        if self.Match(scanner, TokenType::TOKEN_ELSE) {self.statement(scanner);}

        self.patchJump(elseJump);
    }

    fn whileStatement(&mut self, scanner: &mut Scanner){
// Out of bounds problem here        
        let loopStart = self.currentChunk().get_count();
        self.consume(TokenType::TOKEN_LEFT_PAREN, "Expect '(' after 'while'.".to_string(), scanner);
        self.expression(scanner);
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect ')' after 'while'.".to_string(), scanner);
        
        let exitJump = self.emitJump(OpCode::OP_JUMP_IF_FALSE);
        self.emitByte(OpCode::OP_POP as u8);
        self.statement(scanner);
        self.emitLoop(loopStart);
        self.patchJump(exitJump);
        self.emitByte(OpCode::OP_POP as u8);
    }

    fn expression(&mut self, scanner: &mut Scanner,  ) {
        // interpret an expression
        self.parsePrecedence(Precedence::PREC_ASSIGNMENT, scanner);
    }

    fn grouping(&mut self, scanner: &mut Scanner, _canAssign: bool){
        // do a bunch of expression wrapped in parentheses
        self.expression(scanner);
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expected ')' after expression".to_string(), scanner);
    } 

    fn unary(&mut self, scanner: &mut Scanner, _canAssign: bool){
        // once you you hit a unary, you need to process the the following expression while
        // respecting precedence
        let operatorType = self.previous.ttype.clone();
        self.parsePrecedence(Precedence::PREC_UNARY, scanner);
        // after the fact, you apply the unary
        match operatorType {
            TokenType::TOKEN_BANG => {self.emitByte(OpCode::OP_NOT as u8); () },
            TokenType::TOKEN_MINUS => {self.emitByte(OpCode::OP_NEGATE as u8); ()},
            _ => (),
        }
    }
    
    fn binary(&mut self, scanner: &mut Scanner, _canAssign: bool){
        let operatorType = self.previous.ttype.clone();
        let rule = self.getRule(operatorType);
        self.parsePrecedence(rule.unwrap().precedence.decrease_prec().unwrap(), scanner);
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
    
    fn number(&mut self, _scanner: &mut Scanner, _canAssign: bool){
        // cast the previous token's string into a double and add said constant to chunk
        let value: f64 = self.previous.start.parse().unwrap(); 
        self.emitConstant(Value::VAL_NUMBER(value));
    }

    fn and(&mut self, scanner: &mut Scanner,_canAssign: bool){
        let endJump = self.emitJump(OpCode::OP_JUMP_IF_FALSE);
        self.emitByte(OpCode::OP_POP as u8);
        self.parsePrecedence(Precedence::PREC_AND, scanner);
        self.patchJump(endJump);
    }
    
    fn or(&mut self, scanner: &mut Scanner, _canAssign: bool){
        let elseJump = self.emitJump(OpCode::OP_JUMP_IF_FALSE);
        let endJump = self.emitJump(OpCode::OP_JUMP);
        self.patchJump(elseJump);
        self.emitByte(OpCode::OP_POP as u8);
        self.parsePrecedence(Precedence::PREC_OR, scanner);
        self.patchJump(endJump);
    }

    fn literal(&mut self, _scanner: &mut Scanner, _canAssign: bool){
        match self.previous.ttype {
            TokenType::TOKEN_FALSE => {self.emitByte(OpCode::OP_FALSE as u8); ()},
            TokenType::TOKEN_TRUE => {self.emitByte(OpCode::OP_TRUE as u8); ()},
            TokenType::TOKEN_NIL => {self.emitByte(OpCode::OP_NIL as u8); ()},
            _ => {()}
        } 
    }

    fn string(&mut self, _scanner: &mut Scanner, _canAssign: bool){
        let quoted = self.previous.start.clone();
        let len = quoted.len();
        // trim of quotes
        let unquoted = &quoted[1..len-1];
        let new_string = LoxString::new(unquoted.to_string());
        let data =  Value::VAL_OBJ(Rc::new( RefCell::new(new_string)));
        self.emitConstant(data); 
    }

    fn variable(&mut self, scanner: &mut Scanner, canAssign: bool){
        self.namedVariable(self.previous.clone(), scanner, canAssign);
    }

    fn namedVariable(&mut  self, name: Token, scanner: &mut Scanner, canAssign: bool){
        let getOp: OpCode;
        let setOp: OpCode;
        // decide if variable is local or global, and choose the appropriate opcodes
        let arg  = match self.resolveLocal(name.clone()){
            Some(index) => {
                getOp = OpCode::OP_GET_LOCAL;
                setOp = OpCode::OP_SET_LOCAL;
                index
            },
            None => {
                getOp = OpCode::OP_GET_GLOBAL;
                setOp = OpCode::OP_SET_GLOBAL;
                self.identifierConstant(name)
            }
        };

        if self.Match(scanner, TokenType::TOKEN_EQUAL) && canAssign{
            self.expression(scanner);
            self.emitTwoBytes(setOp as u8, arg);
        }
        self.emitTwoBytes(getOp as u8, arg);
    }

    fn resolveLocal(&mut self, name: Token) -> Option<u8>{
        let mut index: isize =  (self.compilerStack.peek_mut().unwrap().localCount as isize) -1;
        loop {
            if index < 0 {
                break;
            }
            let local = self.compilerStack.peek_mut().unwrap().locals.get(index as usize).unwrap();
            if local.name.EqualSemantically(name.clone()) {
                if local.depth.is_none() {
                    self.errorAt("Can't read local variable in it's own initializer".to_string(), ErrorTokenLoc::PREVIOUS);
                }
                return Some(index as u8);                
            }
            index -= 1;
        }
        return None;
    }
    
// the function that handles precedence and allows recursion to occur
    fn parsePrecedence(&mut self, init_precedence: Precedence, scanner: &mut Scanner,){
        self.advance(scanner);
        let canAssign: bool;
        // Check if a prefix operation needs to be performed
        let get_prefix: Option<ParseRule> = self.getRule(self.previous.ttype.clone());
        match get_prefix {
            Some(val) => {
                match val.prefix{
                    // after unwrapping everything, execute the prefix function
                    Some(prefix_func) => {
                        canAssign = init_precedence <= Precedence::PREC_ASSIGNMENT;
                        prefix_func(self, scanner, canAssign);
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
            self.advance(scanner);
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
                Some(function) => function(self, scanner,canAssign),
                None => {
                    self.errorAt("Invalid infix rule".to_string(), ErrorTokenLoc::PREVIOUS);
                    return;
                }
            }
            if canAssign && self.Match(scanner, TokenType::TOKEN_EQUAL){
                self.errorAt("Invalid assignment target".to_string(), ErrorTokenLoc::PREVIOUS);
                return;
            }
        }
    }

    fn parseVariable(&mut self, err_msg: String, scanner: &mut Scanner ) -> u8{
        self.consume(TokenType::TOKEN_IDENTIFIER, err_msg, scanner); 
        self.declareVariable();
        if self.compilerStack.peek_mut().unwrap().scopeDepth > 0 {return 0;}
        return self.identifierConstant(self.previous.clone())
    }

    fn defineVariable(&mut self, global: u8, ){
        if self.compilerStack.peek_mut().unwrap().scopeDepth > 0{
            self.compilerStack.peek_mut().unwrap().markInitialized();
            return; 
        }
        self.emitTwoBytes(OpCode::OP_DEFINE_GLOBAL as u8, global);
    }

    fn declareVariable(&mut self, ){
        if self.compilerStack.peek_mut().unwrap().scopeDepth  == 0 {return ;}
        let sd = self.compilerStack.peek_mut().unwrap().scopeDepth;
        let new_local: Locals = Locals::new(self.previous.clone(), Some(self.compilerStack.peek_mut().unwrap().scopeDepth));
        for index in  (0..self.compilerStack.peek_mut().unwrap().localCount).rev(){
            let local = self.compilerStack.peek_mut().unwrap().locals.get(index as usize).unwrap();
            if local.depth.unwrap() < sd{
                break;
            }
            if  new_local.name.EqualSemantically(local.name.clone()) {
                self.errorAt("Already variable with this name in this scope".to_string(), ErrorTokenLoc::PREVIOUS);
            }
        }
        self.addLocal( new_local);
    }

    fn addLocal(&mut self, mut new_local: Locals){
        new_local.depth = None;
        self.compilerStack.peek_mut().unwrap().localCount += 1;
        self.compilerStack.peek_mut().unwrap().locals.push(new_local);
    }

    fn identifierConstant(&mut self, name: Token) -> u8{
        let str = LoxString::new(name.start);
        let val = Value::VAL_OBJ(Rc::new( RefCell::new(str)));
        self.makeConstant(val)
    }

    fn getRule(&mut self, ttype: TokenType)  -> Option<ParseRule>{
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

    fn emitJump(&mut self, new_val: OpCode) -> usize{
        self.emitByte(new_val as u8);
        self.emitByte(0xff);
        self.emitByte(0xff);
        return self.currentChunk().get_count()-2
    }

    fn patchJump(&mut self, offset: usize){
        let jump = self.currentChunk().get_count()-offset-2;
        let high = jump >> 8 & 0xFF;
        let low  = jump & 0xFF;
        self.currentChunk().edit_chunk(offset, high as u8);
        self.currentChunk().edit_chunk(offset+1, low as u8);
    }

    fn emitLoop(&mut self, loopStart: usize){
        self.emitByte(OpCode::OP_LOOP as u8);
        let offset = self.currentChunk().get_count()- loopStart+2;
        let high = (offset >> 8) & 0xff;
        let low = offset & 0xff;
        self.emitByte(high as u8);
        self.emitByte(low as u8);
    }
    
    pub fn currentChunk(&mut self) -> & mut Chunk{
        match &mut self.compilerStack.peek_mut().unwrap().function{
            Some(fnc) => {
                return &mut fnc.chunk
            },
            None => panic!("Couldn't load function")
        }
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

    fn synchronize(&mut self, scanner: &mut Scanner ){
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
            self.advance(scanner);
        }
    }
   
    fn block(&mut self, scanner: &mut Scanner ){
        loop {
            let cond = self.isAtEnd() || self.check(TokenType::TOKEN_RIGHT_BRACE); 
            if cond {break;}
            self.declaration(scanner);
        }
        self.consume(TokenType::TOKEN_RIGHT_BRACE, "Expect '}' after block.".to_string(), scanner);
    }

    fn function(&mut self, ftype: FunctionType, scanner: &mut Scanner){
/*
 * self.compilerStack.push(Compiler::new(ftype));
        let cur_comp = self.compilerStack.peek_mut().unwrap();
        cur_comp.beginScope();
        // declaration
        self.consume(TokenType::TOKEN_LEFT_PAREN, "Expect '(' after function name.".to_string(), scanner);
        if (! self.check(TokenType::TOKEN_RIGHT_PAREN)){
            while (self.Match(scanner, TokenType::TOKEN_COMMA)){
                cur_comp.function.unwrap().borrow_mut().arity += 1; 
            }
        }
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect ')' after function name.".to_string(), scanner);

        //body
        self.consume(TokenType::TOKEN_LEFT_BRACE, "Expect '{' before function body.".to_string(), scanner);

        let func = self.endParser();
        let new_val = Value::VAL_OBJ(func.unwrap());
        let b = self.makeConstant(new_val);
        self.emitTwoBytes(OpCode::OP_CONSTANT as u8, b);
*/
    }

    fn isAtEnd(&mut self) -> bool{
        return self.current.ttype == TokenType::TOKEN_EOF
    }

    fn check(&mut self, expected: TokenType) -> bool {
        if self.isAtEnd() {return false}
        return self.current.ttype == expected
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
