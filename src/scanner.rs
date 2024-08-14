#![allow(non_camel_case_types)]

pub struct Scanner{
    source: String,
    start: usize,
    current: usize,
    line: usize
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Copy)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
pub enum TokenType{
 // Single-character tokens.
 TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
 TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
 TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
 TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
 // One or two character tokens.
 TOKEN_BANG, TOKEN_BANG_EQUAL,
 TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
 TOKEN_GREATER, TOKEN_GREATER_EQUAL,
 TOKEN_LESS, TOKEN_LESS_EQUAL,
 // Literals.
 TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
 // Keywords.
 TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
 TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
 TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
 TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,
 TOKEN_ERROR,
 TOKEN_EOF
}


#[derive(Clone)]
pub struct Token{
    pub ttype: TokenType,
    pub start: String,
    pub line: usize,
}

impl Scanner{
    pub fn new(src: String) -> Self{
        return Scanner{source: src, start:0, current:0, line: 1};        
    }

    pub fn scanToken(&mut self) -> Token{
        self.skipWhitespace();
        self.start = self.current;
        if self.isAtEnd(){
            return self.make_token(TokenType::TOKEN_EOF);
        }

        let c = match self.advance(){
            Some(val) => val,
            None => return self.error_token(format!("Invalid access in source file")),
        };

        if isAlpha(*c) {return self.identifier();}
        if isDigit(*c) {return self.number();}

        match *c as char{
            '(' => return self.make_token(TokenType::TOKEN_LEFT_PAREN),
            ')' => return self.make_token(TokenType::TOKEN_RIGHT_PAREN),
            '{' => return self.make_token(TokenType::TOKEN_LEFT_BRACE),
            '}' => return self.make_token(TokenType::TOKEN_RIGHT_BRACE),
            ';' => return self.make_token(TokenType::TOKEN_SEMICOLON),
            ',' => return self.make_token(TokenType::TOKEN_COMMA),
            '.' => return self.make_token(TokenType::TOKEN_DOT),
            '-' => return self.make_token(TokenType::TOKEN_MINUS),
            '+' => return self.make_token(TokenType::TOKEN_PLUS),
            '/' => return self.make_token(TokenType::TOKEN_SLASH),
            '*' => return self.make_token(TokenType::TOKEN_STAR),
            '!' => {let final_token = match self.Match('='){true => TokenType::TOKEN_BANG_EQUAL, false => TokenType::TOKEN_BANG}; return self.make_token(final_token);},
            '=' => {let final_token = match self.Match('='){true => TokenType::TOKEN_EQUAL_EQUAL, false => TokenType::TOKEN_EQUAL}; return self.make_token(final_token);},
            '<' => {let final_token = match self.Match('='){true => TokenType::TOKEN_LESS_EQUAL, false => TokenType::TOKEN_LESS}; return self.make_token(final_token);},
            '>' => {let final_token = match self.Match('='){true => TokenType::TOKEN_GREATER_EQUAL, false => TokenType::TOKEN_GREATER}; return self.make_token(final_token);},
            '"' => {return self.string();}
            _ => return self.error_token("Unexpected character.".to_string()),
        }
    }

    pub fn make_token(&self, a_ttype: TokenType) -> Token{
        Token{ttype: a_ttype, start: self.source[self.start..self.current].to_string(), line: self.line}
    } 

    pub fn error_token(&self, message: String) -> Token{
        Token{ttype: TokenType::TOKEN_ERROR, start: message.clone(), line: self.line}
    }

    fn identifier_type(&mut self) -> TokenType{
        match self.source.as_bytes()[self.start] as char{
            'a' => return self.checkKeyword(1, 2, "nd".to_string(), TokenType::TOKEN_AND),
            'c' => return self.checkKeyword(1, 4, "lass".to_string(), TokenType::TOKEN_CLASS),
            'e' => return self.checkKeyword(1, 3, "lse".to_string(), TokenType::TOKEN_ELSE),
            'f' => {
                if unsigned_abs_sub(self.current, self.start) > 1{
                    match self.source.as_bytes()[self.start+1] as char {
                        'a' => return self.checkKeyword(2, 3, "lse".to_string(), TokenType::TOKEN_FALSE),
                        'o' => return self.checkKeyword(2, 1, "r".to_string(), TokenType::TOKEN_OR),
                        'u' => return self.checkKeyword(2, 1, "n".to_string(), TokenType::TOKEN_FUN),
                        _ => return TokenType::TOKEN_IDENTIFIER,
                    }
                }
            }
            'i' => return self.checkKeyword(1, 1, "f".to_string(), TokenType::TOKEN_IF),
            'n' => return self.checkKeyword(1, 2, "il".to_string(), TokenType::TOKEN_NIL),
            'o' => return self.checkKeyword(1, 1, "r".to_string(), TokenType::TOKEN_OR),
            'p' => return self.checkKeyword(1, 4, "rint".to_string(), TokenType::TOKEN_PRINT),
            'r' => return self.checkKeyword(1, 5, "eturn".to_string(), TokenType::TOKEN_RETURN),
            's' => return self.checkKeyword(1, 4, "uper".to_string(), TokenType::TOKEN_SUPER),
            't' => {
                if unsigned_abs_sub(self.current, self.start) > 1{
                    match self.source.as_bytes()[self.start+1] as char {
                        'h' => return self.checkKeyword(2, 2, "is".to_string(), TokenType::TOKEN_THIS),
                        'r' => return self.checkKeyword(2, 2, "ue".to_string(), TokenType::TOKEN_TRUE),
                        _ => return TokenType::TOKEN_IDENTIFIER,
                    }
                }
            }
            'v' => return self.checkKeyword(1, 2, "ar".to_string(), TokenType::TOKEN_VAR),
            'w' => return self.checkKeyword(1, 4, "hile".to_string(), TokenType::TOKEN_WHILE),
            _ => return TokenType::TOKEN_IDENTIFIER,
        }
        return TokenType::TOKEN_IDENTIFIER;
    }

    fn checkKeyword(&self, sub_token_start: usize, sub_token_length: usize, rest: String, ttype: TokenType) -> TokenType{
        let token_length = unsigned_abs_sub(self.current, self.start);
        if token_length == sub_token_start+sub_token_length{
            if self.source.as_bytes()[self.start..self.current] == *rest.as_bytes(){
                return ttype;
            }
        }
        return TokenType::TOKEN_IDENTIFIER;
    }

    pub fn string(&mut self)-> Token{
        let mut is_newline: bool;
        let mut is_string_end: bool;
        loop{
            match self.peek() {
                Some(val) => {
                    match *val as char{
                        '\n' => {is_newline = true; is_string_end= false;},
                        '"' => {is_newline = false; is_string_end= true;},
                        _ => {is_newline = false; is_string_end = false},
                    }          
                },
                None => return self.error_token("Unterminated string.".to_string()),
            }
            if is_string_end || self.isAtEnd() {
                break;
            }
            if is_newline {
                self.line += 1;
                self.advance();
                
            }
        }

        if self.isAtEnd() {return self.error_token("Unterminated string.".to_string());}

        return self.make_token(TokenType::TOKEN_STRING);
    }

    pub fn identifier(&mut self) -> Token {
        let mut consume_var_name = true;
        while consume_var_name {
            match self.peek() {
                Some(val) => {
                    if isAlpha(*val) || isDigit(*val) {self.advance();}
                    else {consume_var_name = false;}
                    ()
                },
                None => {consume_var_name = false; ()}
            } 
        }
        let ttype = self.identifier_type();
        return self.make_token(ttype);
    }

    pub fn number(&mut self) -> Token{
        self.consume_numbers();
        let is_period =  match self.peek(){
            Some(val) => match *val as char{
                '.' => true,
                _ => false,
            },
            None => return self.error_token("Trouble parsing number".to_string()),
        };

        let next_val = match self.peekNext() {
            Some(val) => *val,
            None => return self.error_token("Trouble parsing number".to_string()),
        };

        if is_period && isDigit(next_val){
            self.advance();
            self.consume_numbers();
        }

        return self.make_token(TokenType::TOKEN_NUMBER);
        
    }

    fn consume_numbers(&mut self) -> (){ // true for failure, false for success
        loop {
            let character = match self.peek() {
                Some(val) => { *val  },
                None => return, // reached end of stream, return early
            };
            match isDigit(character) { // advance if character is digit
                true => {self.advance();}
                false =>  {break;}
            }
        }
    }

    fn isAtEnd(&self) -> bool{
        self.source.len() <= self.current
    }

    fn advance(&mut self) -> Option<&u8>{
        self.current += 1;
        return self.source.as_bytes().get(self.current-1);
    }

    fn Match(&mut self, expected: char) -> bool {
        if self.isAtEnd() {return false;}
        if self.source.as_bytes()[self.current] != expected as u8 {return false;}
        self.current += 1;
        return true;
    }

    fn skipWhitespace(&mut self) -> bool{
        loop {
            let c = match self.peek(){
                Some(val) => val,
                None => return true,
            };
            match *c as char {
                ' ' | '\r' | '\t' => match self.advance(){Some(_) => continue, None => return true  },
                '\n' => {
                    self.line += 1;
                    match self.advance(){Some(_) => continue, None => return true}
                },
                '/' => {
                    let comment_start = match self.peekNext(){
                        Some(val) => {
                            match *val as char{
                                '/' => true,
                                _ => false,
                            }
                        },
                        None => false
                    };

                    if comment_start{
                        let mut is_newline: bool;
                        loop{
                            is_newline = match self.peek(){
                                Some(val) => match *val as char{
                                    '\n' => true,
                                    _ => false,
                                },
                                None => false,
                            };
                            if !is_newline && !self.isAtEnd() {self.advance();}
                            else{return true}
                        }
                    } else {return true;}
                }
                _ => return true,
            };
        }
    }

    fn peek(&self) -> Option<&u8>{
        return self.source.as_bytes().get(self.current);
    }

    fn peekNext(&self) -> Option<&u8>{
        if self.isAtEnd() {return None}
        return self.source.as_bytes().get(self.current+1);
    }
}

pub fn isDigit(character: u8) -> bool{
        let c = character as char;
        return c >= '0' && c <= '9';
}

pub fn isAlpha(character: u8) -> bool {
        let c = character as char;
        return (c>='a' && c <='z') || (c>='A' && c <='Z') || (c == '_');
}

pub fn unsigned_abs_sub(left: usize, right: usize) -> usize{
    return left.checked_sub(right).unwrap_or(right-left);
}
