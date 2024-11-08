#![allow(non_camel_case_types)]
use crate::scanner::{Token, TokenType, Scanner};
use crate::chunk::{Chunk, Constant, OpCode};
use crate::value::*;
use crate::DEBUG_PRINT_CODE;
use std::mem;

// size of ParseRules lookup table
const TOTAL_RULES: usize = 40;

// This only exists so that you can tell when you are at the top of the call stack
#[derive(PartialEq)]
pub enum FunctionType {
    TYPE_FUNCTION,
    TYPE_METHOD,
    TYPE_SCRIPT,
}


pub struct Compiler {
    pub function: LoxFunction, // what function the compiler is currently trying to check
    ftype: FunctionType, // tells you if the current function is main
    locals: Vec<Locals>, // a stack of local variables that are accessible in the current scope
    scopeDepth: u64, // the current scope
    upvalues: Vec<UpvalueIndex>  // upvalues needed by function
}

impl Compiler {
    pub fn new(new_ftype: FunctionType, fname: String) -> Self{
       match new_ftype{
            FunctionType::TYPE_FUNCTION => {
                let temp_token = Token{ttype: TokenType::TOKEN_THIS, start: "".to_string(), line: 0};
                let new_locals = vec![Locals::new( temp_token, Some(0))];
                Compiler{
                    function: LoxFunction::new(0, fname),
                    ftype: new_ftype,
                    locals: new_locals,
                    scopeDepth: 0,
                    upvalues: Vec::new()
                }
            },
            // the "main" function goes here
            FunctionType::TYPE_SCRIPT => {
                let temp_token = Token{ttype: TokenType::TOKEN_THIS, start: "".to_string(), line: 0};
                let new_locals = vec![Locals::new( temp_token, Some(0))];
                Compiler{
                    function: LoxFunction::new(0, "script".to_string()),
                    ftype: new_ftype,
                    locals: new_locals,
                    scopeDepth: 0,
                    upvalues: Vec::new()
                }
            },
            FunctionType::TYPE_METHOD => {
                let temp_token = Token{ttype: TokenType::TOKEN_THIS, start: "this".to_string(), line: 0};
                let new_locals = vec![Locals::new( temp_token, Some(0))];
                Compiler{
                    function: LoxFunction::new(0, fname),
                    ftype: new_ftype,
                    locals: new_locals,
                    scopeDepth: 0,
                    upvalues: Vec::new()
                }
            }
        }
       
    }

    pub fn beginScope(&mut self){
        self.scopeDepth += 1;
    }
    
    pub fn endScope(&mut self){
        self.scopeDepth -= 1;
    }
   
    fn markInitialized(& mut self) {
        if self.scopeDepth == 0 {return}
        // utilized to prevent self-initialization (ie. var a = a; type of thing).
        // Use option as a sentinel value
        self.locals.last_mut().unwrap().depth = 
            Some(self.scopeDepth);
    }
}

// defines a local variable
pub struct Locals {
    pub name: Token, // name of variable
    pub depth: Option<u64>, // scope of local
    pub isCaptured: bool    // weather a local variable gets captured
}

impl Locals {
    pub fn new(new_name: Token, new_depth: Option<u64>) -> Self{
        Locals{name: new_name, depth: new_depth, isCaptured: false}
    }
}

pub struct ClassCompilerNode{
    pub name: Token
}

// the beating heart of the interpreter
// requests tokens as needed from a scanner, then converts them too bytecode
// It's basically as single pass compiler
pub struct Parser{
    compilerStack: Vec<Compiler>, // current chunk being executed. Could represent a function, or
                                    // a group of statements
    ClassStack: Vec<ClassCompilerNode>, 
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

// Precedence of operations. Goes from high precedence to low precedence
// Bigger number is lower precedence
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
 rules[TokenType::TOKEN_LEFT_PAREN as usize] = ParseRule{prefix: Some(Parser::grouping), infix: Some(Parser::call), precedence: Precedence::PREC_CALL};
 rules[TokenType::TOKEN_RIGHT_PAREN as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_LEFT_BRACE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_RIGHT_BRACE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_COMMA as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_DOT as usize] = ParseRule{prefix: None, infix: Some(Parser::dot), precedence: Precedence::PREC_CALL};
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
 rules[TokenType::TOKEN_THIS as usize] = ParseRule{prefix: Some(Parser::this), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_TRUE as usize] = ParseRule{prefix: Some(Parser::literal), infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_VAR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_WHILE as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_ERROR as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
 rules[TokenType::TOKEN_EOF as usize] = ParseRule{prefix: None, infix: None, precedence: Precedence::PREC_NONE};
   
    let mut a: Vec<Compiler> = Vec::new(); 
    a.push(Compiler::new(FunctionType::TYPE_SCRIPT, "script".to_string()));
 
 Parser{compilerStack: a,
        ClassStack: Vec::new(), 
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

        // A program is just a series of declarations that ends in a TOKEN_EOF
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

// Stream manipulation functions
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
 
    fn isAtEnd(&mut self) -> bool{
        return self.current.ttype == TokenType::TOKEN_EOF
    }

    fn check(&mut self, expected: TokenType) -> bool {
        // Check if you have the specified token without consuming it
        if self.isAtEnd() {return false}
        return self.current.ttype == expected
    }

// grammar production functions
// see https://craftinginterpreters.com/image/compiling-expressions/connections.png
// These are effectively translating the BNF notation of Lox into code

    fn declaration(&mut self, scanner: &mut Scanner, ){
        // a statement which binds other statements
        // You can think of bindings as associating a variable with a specific scope
        // The top level entry point.
        if self.Match(scanner, TokenType::TOKEN_FUN){
            self.funDeclaration(scanner);
        } else if self.Match(scanner, TokenType::TOKEN_VAR){
            self.varDeclaration(scanner);
        } else if self.Match(scanner, TokenType::TOKEN_CLASS){
            self.classDeclaration(scanner);
        }
        else {
            self.statement(scanner); 
        }
        if self.panicMode {
            self.synchronize(scanner);
        }
    }
    
    fn statement(&mut self, scanner: &mut Scanner ){
        // Rules which produce side effects, but don't introduce bindings
        // Most of these just point to a specific function to handling parsing
        if self.Match(scanner, TokenType::TOKEN_PRINT){
            self.printStatement(scanner); 
        } else if self.Match(scanner, TokenType::TOKEN_FOR){
            self.forStatement(scanner);
        }
        else if self.Match(scanner, TokenType::TOKEN_IF){
            self.ifStatement(scanner );
        }
        else if self.Match(scanner, TokenType::TOKEN_RETURN){
            self.returnStatement(scanner);
        } else if self.Match(scanner, TokenType::TOKEN_WHILE){
            self.whileStatement(scanner);
        } 
        else if self.Match(scanner, TokenType::TOKEN_LEFT_BRACE){
            // Blocks are slightly more involved
            // You need to wrap the block in it's own scope
            self.beginScope();
            self.block(scanner);
           self.endScope();
        }
        else {
            self.expressionStatement(scanner);
        }
    }
    
    fn expression(&mut self, scanner: &mut Scanner,  ) {
        // interpret an expression
        self.parsePrecedence(Precedence::PREC_ASSIGNMENT, scanner);
    }

    fn expressionStatement(&mut self, scanner: &mut Scanner ){
        // This just parses an expression, but demands that there's a ; afterwards
        self.expression(scanner);
        self.consume(TokenType::TOKEN_SEMICOLON, "Expect ; after value.".to_string(), scanner);
        self.emitByte(OpCode::OP_POP);
    }

    fn varDeclaration(&mut self, scanner: &mut Scanner,  ){
        // Declares a variable
        // BNF notation: "var" IDENTIFIER ( "=" expression )? ";"
        // This allows you to declare an uninitialized and initialized variable;
        
        // Parse the variable declaration part 
        let global = self.parseVariable("Expect variable name".to_string(), scanner);
        // see if you need to assign a value to the variable right now
        if self.Match(scanner, TokenType::TOKEN_EQUAL) {
            self.expression(scanner);
        } else {
            self.emitByte(OpCode::OP_NIL);
        }
        self.consume(TokenType::TOKEN_SEMICOLON, " Expect ';' after variable declaration.".to_string(), scanner);
        // Add the variable to the VM hashmap
        self.defineVariable(global);
    }

    fn funDeclaration(&mut self, scanner: &mut Scanner){
        // Declare the existence of a function
        // BNF: "fun" function 
        // Get the name of the function
        let global = self.parseVariable("Expect function name.".to_string(), scanner);
        // You shouldn't be able to self-assign a function
        self.compilerStack.last_mut().unwrap().markInitialized();
        self.function(FunctionType::TYPE_FUNCTION, scanner);
        self.defineVariable(global);
    }

    fn classDeclaration(&mut self, scanner: &mut Scanner){
        self.consume(TokenType::TOKEN_IDENTIFIER, "Expect class name".to_string(), scanner);
        let className = self.previous.clone();
        let nameConstant = self.identifierConstant(self.previous.clone());
        self.declareVariable();

        self.emitByte(OpCode::OP_CLASS(nameConstant));
        self.defineVariable(nameConstant);

        self.ClassStack.push(ClassCompilerNode{name: self.previous.clone()});

        self.namedVariable(className, scanner, false);

        self.consume(TokenType::TOKEN_LEFT_BRACE, "Expect '{' before class body.".to_string(), scanner);
        loop{
            let condition = self.check(TokenType::TOKEN_RIGHT_BRACE) || self.check(TokenType::TOKEN_EOF);
            if condition {break;}
            self.method(scanner);
        }

        self.consume(TokenType::TOKEN_RIGHT_BRACE, "Expect '}' before class body.".to_string(), scanner);
        self.emitByte(OpCode::OP_POP);

        self.ClassStack.pop();
    }
    
    fn printStatement(&mut self, scanner: &mut Scanner){
        // BNF: "print" expression ";"
        self.expression(scanner);
        self.consume(TokenType::TOKEN_SEMICOLON, "Expect ; after value.".to_string(), scanner);
        self.emitByte(OpCode::OP_PRINT);
    }

    fn forStatement(&mut self, scanner: &mut Scanner){
        // BNF: "for" "(" ( varDecl | exprStmt | ";" )
        //                   expression? ";"
        //                   expression? ")" statement 
        //  for statement has it's own scope. Done here to include looping variables in scope
        self.beginScope(); 
        self.consume(TokenType::TOKEN_LEFT_PAREN, "Expect '(' after 'for'.".to_string(), scanner);
        // parsing initialization condition
        if self.Match(scanner, TokenType::TOKEN_SEMICOLON){}
        else if self.Match(scanner, TokenType::TOKEN_VAR){
            self.varDeclaration(scanner);
        } else {
            self.expressionStatement(scanner);
        }
        
        // doing backpatching for the looping part since this is a single pass compiler
        // This means you can't look ahead to find the end of the loop
        // You can omit the condition for some reason.

        // get starting address of loop
        let mut loopStart = self.currentChunk().get_count();

        // check if you need to be able to exit. If so, backpatch
        let exitJump: Option<usize>;
        if !self.Match(scanner, TokenType::TOKEN_SEMICOLON) {
            self.expression(scanner);
            self.consume(TokenType::TOKEN_SEMICOLON, "Expect ';' after condition.".to_string(), scanner);
            
            exitJump =  Some(self.emitJump(OpCode::OP_JUMP_IF_FALSE(0)));
            self.emitByte(OpCode::OP_POP);
        } else{
            exitJump = None;
        }
        
        // check if there needs to be a body. If so, backpatch
        if  !self.Match(scanner, TokenType::TOKEN_RIGHT_PAREN) {
            let bodyJump = self.emitJump(OpCode::OP_JUMP(0));
            let incrementStart = self.currentChunk().get_count();
            self.expression(scanner);
            self.emitByte(OpCode::OP_POP);
            self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect ')' after 'for'.".to_string(), scanner);

            self.emitLoop(loopStart);
            loopStart = incrementStart;

            self.patchJump(bodyJump);
        }

        // The meat of the loop
        self.statement(scanner);

        // back patching the loop
        self.emitLoop(loopStart);
        // patching the exit condition
        match exitJump {
            Some(offset) => {self.patchJump(offset); self.emitByte(OpCode::OP_POP); ()}
            None => ()
        }

        // for loops close scoping once done
        self.endScope();

    }

    fn ifStatement(&mut self, scanner: &mut Scanner){
        // back patching if and then jumps
        self.consume(TokenType::TOKEN_LEFT_PAREN, "Expect '(' after 'if'.".to_string(), scanner);
        self.expression(scanner);
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect '(' after condition.".to_string(), scanner);
        let thenJump: usize = self.emitJump(OpCode::OP_JUMP_IF_FALSE(0)); 
        self.emitByte(OpCode::OP_POP);
        self.statement(scanner);
        
        let elseJump: usize = self.emitJump(OpCode::OP_JUMP(0));
        self.patchJump(thenJump);
        self.emitByte(OpCode::OP_POP);

        if self.Match(scanner, TokenType::TOKEN_ELSE) {self.statement(scanner);}

        self.patchJump(elseJump);
    }

    fn returnStatement(&mut self, scanner: &mut Scanner){
        // make sure you can't return from top level
        if self.compilerStack.last_mut().unwrap().ftype
            == FunctionType::TYPE_SCRIPT{
            self.errorAt("Can't return from top-level code;".to_string(), ErrorTokenLoc::PREVIOUS);
        }
        //Can return a nill value like: return ;
        if self.Match(scanner, TokenType::TOKEN_SEMICOLON) {
            self.emitReturn();
        } else {
            // otherwise, can return top of stack
            self.expression(scanner);
            self.consume(TokenType::TOKEN_SEMICOLON, "Expect ';' after return value".to_string(), scanner);
            self.emitByte(OpCode::OP_RETURN);
        }
    }

    fn whileStatement(&mut self, scanner: &mut Scanner){
        let loopStart = self.currentChunk().get_count();
        self.consume(TokenType::TOKEN_LEFT_PAREN, "Expect '(' after 'while'.".to_string(), scanner);
        self.expression(scanner);
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect ')' after 'while' expression.".to_string(), scanner);
       
        // more backpatching stuff
        let exitJump = self.emitJump(OpCode::OP_JUMP_IF_FALSE(0));
        self.emitByte(OpCode::OP_POP);
        self.statement(scanner);
        self.emitLoop(loopStart);
        self.patchJump(exitJump);
        self.emitByte(OpCode::OP_POP );
    }

    fn grouping(&mut self, scanner: &mut Scanner, _canAssign: bool){
        // do a bunch of expression wrapped in parentheses
        self.expression(scanner);
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expected ')' after expression".to_string(), scanner);
    } 

    fn call(&mut self, scanner: &mut Scanner, _canAssign: bool){
        // parse the arguments, and then emit call opcode
        let argCount = self.argumentList(scanner);
        self.emitByte(OpCode::OP_CALL(argCount as usize));
    }

    fn dot(&mut self, scanner: &mut Scanner, _canAssign: bool){
        self.consume(TokenType::TOKEN_IDENTIFIER, "Expect property name after '.'.".to_string(), scanner);
        let name = self.identifierConstant(self.previous.clone());
        if _canAssign && self.Match(scanner, TokenType::TOKEN_EQUAL){
            self.expression(scanner);
            self.emitByte(OpCode::OP_SET_PROPERTY(name));
        } else{
            self.emitByte(OpCode::OP_GET_PROPERTY(name));
        }
    }

    fn unary(&mut self, scanner: &mut Scanner, _canAssign: bool){
        // once you you hit a unary, you need to process the the following expression while
        // respecting precedence
        let operatorType = self.previous.ttype.clone();
        self.parsePrecedence(Precedence::PREC_UNARY, scanner);
        // after the fact, you apply the unary
        match operatorType {
            TokenType::TOKEN_BANG => {self.emitByte(OpCode::OP_NOT ); () },
            TokenType::TOKEN_MINUS => {self.emitByte(OpCode::OP_NEGATE ); ()},
            _ => (),
        }
    }
    
    fn binary(&mut self, scanner: &mut Scanner, _canAssign: bool){
        let operatorType = self.previous.ttype.clone();
        let rule = self.getRule(operatorType);
        self.parsePrecedence(rule.unwrap().precedence.decrease_prec().unwrap(), scanner);
        match operatorType {
            TokenType::TOKEN_BANG_EQUAL => {self.emitTwoBytes(OpCode::OP_EQUAL , OpCode::OP_NOT ); ()},
            TokenType::TOKEN_EQUAL_EQUAL => {self.emitByte(OpCode::OP_EQUAL ); ()},
            TokenType::TOKEN_GREATER => {self.emitByte(OpCode::OP_GREATER ); ()},
            TokenType::TOKEN_GREATER_EQUAL => {self.emitTwoBytes(OpCode::OP_LESS , OpCode::OP_NOT ); ()},
            TokenType::TOKEN_LESS => {self.emitByte(OpCode::OP_LESS ); ()},
            TokenType::TOKEN_LESS_EQUAL => {self.emitTwoBytes(OpCode::OP_GREATER , OpCode::OP_NOT ); ()},
            TokenType::TOKEN_PLUS => {self.emitByte(OpCode::OP_ADD ); ()},
            TokenType::TOKEN_MINUS => {self.emitByte(OpCode::OP_SUBTRACT ); ()},
            TokenType::TOKEN_STAR => {self.emitByte(OpCode::OP_MULTIPLY ); ()},
            TokenType::TOKEN_SLASH => {self.emitByte(OpCode::OP_DIVIDE ); ()},
            _ => (),
        }
    }
    
    fn number(&mut self, _scanner: &mut Scanner, _canAssign: bool){
        // cast the previous token's string into a double and add said constant to chunk
        let value: f64 = self.previous.start.parse().unwrap(); 
        self.emitConstant(Constant::NUMBER(value));
    }

    fn and(&mut self, scanner: &mut Scanner,_canAssign: bool){
        let endJump = self.emitJump(OpCode::OP_JUMP_IF_FALSE(0));
        self.emitByte(OpCode::OP_POP);
        self.parsePrecedence(Precedence::PREC_AND, scanner);
        self.patchJump(endJump);
    }
    
    fn or(&mut self, scanner: &mut Scanner, _canAssign: bool){
        let elseJump = self.emitJump(OpCode::OP_JUMP_IF_FALSE(0));
        let endJump = self.emitJump(OpCode::OP_JUMP(0));
        self.patchJump(elseJump);
        self.emitByte(OpCode::OP_POP);
        self.parsePrecedence(Precedence::PREC_OR, scanner);
        self.patchJump(endJump);
    }

    fn literal(&mut self, _scanner: &mut Scanner, _canAssign: bool){
        match self.previous.ttype {
            TokenType::TOKEN_FALSE => {self.emitByte(OpCode::OP_FALSE); ()},
            TokenType::TOKEN_TRUE => {self.emitByte(OpCode::OP_TRUE); ()},
            TokenType::TOKEN_NIL => {self.emitByte(OpCode::OP_NIL); ()},
            _ => {()}
        } 
    }

    fn this(&mut self, scanner: &mut Scanner, _canAssign: bool){
        if self.ClassStack.len() == 0 {
            self.errorAt("Can't use 'this' outside of a class".to_string(), ErrorTokenLoc::PREVIOUS);
            return;
        }
        self.variable(scanner, false);
    }

    fn string(&mut self, _scanner: &mut Scanner, _canAssign: bool){
        let quoted = self.previous.start.clone();
        let len = quoted.len();
        // trim of quotes
        let unquoted = &quoted[1..len-1];
        let new_string = unquoted.to_string();
        let data =  Constant::STRING(new_string);
        self.emitConstant(data); 
    }

    fn variable(&mut self, scanner: &mut Scanner, canAssign: bool){
        self.namedVariable(self.previous.clone(), scanner, canAssign);
    }

    fn namedVariable(&mut  self, name: Token, scanner: &mut Scanner, canAssign: bool){
        let getOp: OpCode;
        let setOp: OpCode;
        let top_compiler_index = self.compilerStack.len()-1;
        // decide if variable is local or global, and choose the appropriate opcodes
        let arg  = match self.resolveLocal(top_compiler_index, name.clone()){
            Some(index) => {
                getOp = OpCode::OP_GET_LOCAL(0);
                setOp = OpCode::OP_SET_LOCAL(0);
                index
            },
            None => {
                match self.resolveUpvalue(top_compiler_index, name.clone()){
                    Some(i) => {
                        getOp  = OpCode::OP_GET_UPVALUE(0); 
                        setOp  = OpCode::OP_SET_UPVALUE(0); 
                        i
                    },
                    None => {
                        getOp = OpCode::OP_GET_GLOBAL(0);
                        setOp = OpCode::OP_SET_GLOBAL(0);
                        self.identifierConstant(name) 
                    }
                }    
            }
        };

        if self.Match(scanner, TokenType::TOKEN_EQUAL) && canAssign{
            self.expression(scanner);
            match setOp {
                OpCode::OP_SET_GLOBAL(_) => self.emitByte(OpCode::OP_SET_GLOBAL(arg as usize)),
                OpCode::OP_SET_LOCAL(_) => self.emitByte(OpCode::OP_SET_LOCAL(arg as usize)),
                OpCode::OP_SET_UPVALUE(_) => self.emitByte(OpCode::OP_SET_UPVALUE(arg as usize)),
                _ => panic!()
            }
        }
        match getOp {
            OpCode::OP_GET_GLOBAL(_) => self.emitByte(OpCode::OP_GET_GLOBAL(arg as usize)),
            OpCode::OP_GET_LOCAL(_) => self.emitByte(OpCode::OP_GET_LOCAL(arg as usize)),
            OpCode::OP_GET_UPVALUE(_) => {self.emitByte(OpCode::OP_GET_UPVALUE(arg as usize)); },
            _ => panic!()
        }
    }

    fn resolveLocal(&mut self,compiler_stack_index: usize, name: Token) -> Option<usize>{
        // walk up the compiler stack until you hit the top or you find a variable with the
        // semantically matching name
        let cur_comp = self.compilerStack.get_mut(compiler_stack_index).unwrap();
        let mut index: isize =  (cur_comp.locals.len() as isize) -1;
        loop {
            if index < 0 {
                break;
            }
            let local = cur_comp.locals.get(index as usize).unwrap();
            if local.name.EqualSemantically(name.clone()) {
                if local.depth.is_none() {
                    // prevent self initialization
                    self.errorAt("Can't read local variable in it's own initializer".to_string(), ErrorTokenLoc::PREVIOUS);
                }
                return Some(index as usize); 
            }
            index -= 1;
        }
        return None;
    }

    fn resolveUpvalue(&mut self, compiler_index: usize, name: Token) -> Option<usize> {
        // If at base of compilerStack, then you can reference a wrapping closure
        if compiler_index == 0 {return None}
        match self.resolveLocal(compiler_index-1, name.clone()){
            Some(local) =>{
                self.compilerStack.get_mut(compiler_index-1).unwrap().locals[local as usize].isCaptured = true;
                return Some(self.addUpvalue(compiler_index, local+1 as usize, UpvalueType::LOCAL))
            },
            None => {
                let upvalue = self.resolveUpvalue(compiler_index-1, name);
                match upvalue{
                    Some(val) => return Some(self.addUpvalue(compiler_index, val as usize, UpvalueType::UPVALUE)),
                    None => return None
                }
            } 
        }
    }

    fn addUpvalue(&mut self, compiler_index: usize,  index: usize, isLocal: UpvalueType ) -> usize{
        let comp = self.compilerStack.get_mut(compiler_index).unwrap();
        for i in 0..comp.upvalues.len(){
            if(comp.upvalues[i].index == index) && (comp.upvalues[i].isLocal == isLocal){
                return i
            }
       }
        if comp.upvalues.len()+1 >= 255 {
            self.errorAt("Too many closure variables in function".to_string(), ErrorTokenLoc::PREVIOUS);
            return 0;
        }
        let v = UpvalueIndex{index, isLocal}; 
        comp.upvalues.push(v);
        comp.function.upvalueCount = comp.upvalues.len();
        return comp.function.upvalueCount-1 
   }
    
// the function that handles precedence and allows recursion to occur
    fn parsePrecedence(&mut self, init_precedence: Precedence, scanner: &mut Scanner,){
        self.advance(scanner);
        let canAssign: bool;
        // Check if a prefix operation needs to be performed
        let get_prefix: Option<ParseRule> = self.getRule(self.previous.ttype.clone());
        match get_prefix {
            Some(val) => {
                let prefix_func = val.prefix.expect("Somethingg went horribly wrong while parsing");
                canAssign = init_precedence <= Precedence::PREC_ASSIGNMENT;
                prefix_func(self, scanner, canAssign);
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

            // execute the infix function
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

    fn parseVariable(&mut self, err_msg: String, scanner: &mut Scanner ) -> usize{
        self.consume(TokenType::TOKEN_IDENTIFIER, err_msg, scanner); 
        self.declareVariable();
        if self.compilerStack.last_mut().unwrap().scopeDepth > 0 {return 0;}
        return self.identifierConstant(self.previous.clone())
    }

    fn defineVariable(&mut self, global: usize ){
        // emit variable to vm
        if self.compilerStack.last_mut().unwrap().scopeDepth > 0{
            self.compilerStack.last_mut().unwrap().markInitialized();
            return; 
        }
        self.emitByte(OpCode::OP_DEFINE_GLOBAL(global as usize));
    }

    fn declareVariable(&mut self, ){
        // walk up compiler stack and check if variable already exists in the current scope
        if self.compilerStack.last_mut().unwrap().scopeDepth  == 0 {return ;}
        let sd = self.compilerStack.last_mut().unwrap().scopeDepth;
        let new_local: Locals = Locals::new(self.previous.clone(), Some(self.compilerStack.last_mut().unwrap().scopeDepth));
        for index in  (0..self.compilerStack.last_mut().unwrap().locals.len()).rev(){
            let local = self.compilerStack.last_mut().unwrap().locals.get(index as usize).unwrap();
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
        // add local variable to current compiler
        new_local.depth = None;
        self.compilerStack.last_mut().unwrap().locals.push(new_local);
    }

    fn identifierConstant(&mut self, name: Token) -> usize{
        // add variable name to chunk
        let val = Constant::STRING(name.start);
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

    fn emitByte(&mut self, new_opcode: OpCode){
        // add a byte to the current chunk
        let line = self.previous.line;
        self.currentChunk().write_chunk(new_opcode, line);
    }

    fn emitTwoBytes(&mut self, code1: OpCode, code2: OpCode){
        self.emitByte(code1);
        self.emitByte(code2);
    }

    fn emitConstant(&mut self, value: Constant){
        // adds the opcode and the constant index
        let cnst = self.makeConstant(value);
        self.emitByte(OpCode::OP_CONSTANT(cnst ));
    }

    fn makeConstant(&mut self, cnst: Constant) -> usize{
       let cnst_index = self.currentChunk().add_constant(cnst); 
        // arbitrarily cap number of constants at u8::MAX because you have to draw the line
        // somewhere
        if cnst_index > std::u8::MAX as usize {
            self.errorAt("Too many constants in one chunk".to_string(), ErrorTokenLoc::PREVIOUS);
            return 0;
        }
        return cnst_index;
    }

    fn emitReturn(&mut self) {
        // you emit a OP_NIL since you need to cover the case where nothing gets returned
        self.emitByte(OpCode::OP_NIL);
        self.emitByte(OpCode::OP_RETURN);
    }

    fn emitJump(&mut self, new_val: OpCode) -> usize{
        // Emit a jump instruction
        match new_val {
            OpCode::OP_LOOP(_) => self.emitByte(OpCode::OP_LOOP(0xFFFF)),
            OpCode::OP_JUMP(_) => self.emitByte(OpCode::OP_JUMP(0xFFFF)),
            OpCode::OP_JUMP_IF_FALSE(_) => self.emitByte(OpCode::OP_JUMP_IF_FALSE(0xFFFF)),
            _ => panic!()
        }
        // return current index of instruction for patching later
        return self.currentChunk().get_count()-1
    }

    fn patchJump(&mut self, offset: usize){
        // override the previous two bytes with the new offset
        let jump = self.currentChunk().get_count()-offset;
        let a = self.currentChunk().get_instr(offset);
        match a.unwrap() {
            OpCode::OP_LOOP(_) => self.currentChunk().edit_chunk(offset, OpCode::OP_LOOP(jump)),
            OpCode::OP_JUMP(_) => self.currentChunk().edit_chunk(offset, OpCode::OP_JUMP(jump)),
            OpCode::OP_JUMP_IF_FALSE(_) => self.currentChunk().edit_chunk(offset, OpCode::OP_JUMP_IF_FALSE(jump)),
            _ => panic!()
        }
    }

    fn emitLoop(&mut self, loopStart: usize){
        // backpatch loop, like patchJump
        let offset = self.currentChunk().get_count()-loopStart;
        self.emitByte(OpCode::OP_LOOP(offset));
    }
    
    pub fn currentChunk(&mut self) -> & mut Chunk{
        // utility function to get current chunk
        return &mut self.compilerStack.last_mut().unwrap().function.chunk;
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
                eprint!(" at end. ");
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
        // scramble ahead to next possibly valid token
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
        // consume all declarations until right brace encountered
        loop {
            let cond = self.isAtEnd() || self.check(TokenType::TOKEN_RIGHT_BRACE); 
            if cond {break;}
            self.declaration(scanner);
        }
        self.consume(TokenType::TOKEN_RIGHT_BRACE, "Expect '}' after block.".to_string(), scanner);
    }

    fn function(&mut self, ftype: FunctionType, scanner: &mut Scanner){
        // BNF: IDENTIFIER "(" parameters? ")" block ;
        // IDENTIFIER was parsed in funDecl, so that's fine

        // Entering a new function means you need a new compiler
        self.compilerStack.push(Compiler::new(ftype, self.previous.start.clone()));
        self.beginScope();
        self.consume(TokenType::TOKEN_LEFT_PAREN, "Expect '(' after function name.".to_string(), scanner);
        // You parse arguments of the function, checking to make sure that you don't have a
        // stupidly large number of args and that each argument is a valid variable
        if ! self.check(TokenType::TOKEN_RIGHT_PAREN) {
            loop {
                self.compilerStack.last_mut().unwrap().function.arity += 1;
                if self.compilerStack.last_mut().unwrap().function.arity >= 255 {
                    self.errorAt("Can't have more than 255 parameters".to_string(), ErrorTokenLoc::CURRENT);
                }
                let paramConstant = self.parseVariable("Expect parameter name".to_string(), scanner);
                self.defineVariable(paramConstant);
                if !self.Match(scanner, TokenType::TOKEN_COMMA){
                    break;
                }
            }
        }
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect ')' after arguments.".to_string(), scanner);

        self.consume(TokenType::TOKEN_LEFT_BRACE, "Expect '{' before function body.".to_string(), scanner);

        // block consumes '}', so don't worry about that
        self.block(scanner);
        self.endScope();


        // Return from the function an store value on stack
        let comp = self.endParser().unwrap();

        // save closure to vm
        let new_val = Constant::FUNCTION(comp.function);
        let b = self.makeConstant(new_val);
        self.emitByte(OpCode::OP_CLOSURE(b as usize, comp.upvalues) );
    }

    fn method(&mut self, scanner: &mut Scanner){
        self.consume(TokenType::TOKEN_IDENTIFIER, "Expect method name.".to_string(), scanner);
        let constant = self.identifierConstant(self.previous.clone());
        self.function(FunctionType::TYPE_METHOD, scanner);
        self.emitByte(OpCode::OP_METHOD(constant));
    }

    fn beginScope(&mut self){
        self.compilerStack.last_mut().unwrap().beginScope();
    }

    fn endScope(&mut self){
        self.compilerStack.last_mut().unwrap().endScope();
        loop {
                // Once the block is done, you need to pop off all of the local variables which
                // where generated in the block

                let cur_comp = self.compilerStack.last().unwrap();
                if cur_comp.locals.len() == 0 {
                    break;
                }
                // you get rid of all variables whose depth is greater than that of the current
                // scope depth
                let var_depth = cur_comp.locals.last().unwrap().depth.unwrap();
                if var_depth > cur_comp.scopeDepth {
                    if cur_comp.locals.last().unwrap().isCaptured{
                        self.emitByte(OpCode::OP_CLOSE_UPVALUE);
                    } else {
                        self.emitByte(OpCode::OP_POP);
                    }
                    self.compilerStack.last_mut().unwrap().locals.pop();
                    continue;
                }
                break;
        } 
    }

    fn endParser(&mut self) -> Option<Compiler>{
        // tops off current chunk with return, disassembles if prompted, then pops the compiler
        // stack to deal with the next function
        self.emitReturn();
        let function = &self.compilerStack.last_mut().unwrap().function;
        if *DEBUG_PRINT_CODE.get().unwrap(){
            if !self.hadError{
                let msg = function.name.clone();
                let _ = self.currentChunk().disassemble(msg);
            }
        }
        // move up to new compiler
        let out = self.compilerStack.pop();
       return out;
    }

    fn argumentList(&mut self, scanner: &mut Scanner) -> u8{
        // parse arguments of function, checking for arity
        let mut argCount = 0;
        if !self.check(TokenType::TOKEN_RIGHT_PAREN) {
            loop{
                self.expression(scanner);
                if argCount == 255 {
                    self.errorAt("Can't have more than 255 arguments.".to_string(), ErrorTokenLoc::PREVIOUS);
                }
                argCount += 1;
                if !self.Match(scanner,TokenType::TOKEN_COMMA){break;}
            }
        }
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect ')' after arguments when calling function.".to_string(), scanner);
        return argCount;
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
