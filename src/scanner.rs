#![allow(non_camel_case_types)]

// scanner class which takes in a string and spits out tokens on demand
pub struct Scanner{
    source: String, // original source code
    start: usize, // the starting index of the current token 
    current: usize, // the current index of the current token (ie. the next character to process)
    line: usize // the current line number
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
 // Special end of stream token
 TOKEN_EOF
}


#[derive(Clone)]
pub struct Token{
    pub ttype: TokenType,
    pub start: String, // the full string associated with the token
    pub line: usize, // the line number of the token
}

impl Scanner{
    pub fn new(src: String) -> Self{
        return Scanner{source: src, start:0, current:0, line: 1};        
    }

    pub fn scanToken(&mut self) -> Token{
        self.skipWhitespace();
        self.start = self.current; // realign start of token to current token
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

    // token generators
    fn identifier(&mut self) -> Token {
        // identifiers follow the regex [a-zA-z_0-9]+
        let mut consume_var_name = true;
        while consume_var_name {
            match self.peek() {
                None => {consume_var_name = false; ()},
                Some(val) => {
                    if isAlpha(*val) || isDigit(*val) {
                        self.advance();
                        continue;
                    }
                    consume_var_name = false;
                    ()
                }
            } 
        }
        let ttype = self.identifier_type();
        return self.make_token(ttype);
    }

    fn number(&mut self) -> Token{
        // consume the first part of a (potential) floating number
        self.consume_numbers();
        // check for period
        let is_period =  match self.peek(){
            Some(val) => match *val as char{
                '.' => true,
                _ => false,
            },
            None => return self.error_token("Trouble parsing number. Invalid index".to_string()),
        };

        // no period found means we have an integer! so we can stop early
        if !is_period {
            return self.make_token(TokenType::TOKEN_NUMBER);
        }

        // decimal found. need to check if there are things after the decimal
        let next_val = match self.peekNext() {
            Some(val) => *val,
            None => return self.error_token("Trouble parsing number after decimal. Invalid Index".to_string()),
        };

        // so a period was found, but afterwards, it ain't a digit. So it's possible the period is
        // part of the next token. Hence, we DON'T consume it, and instead tokenize only the
        // numbers before it
        if !isDigit(next_val){
            return self.make_token(TokenType::TOKEN_NUMBER);
        }
        
        // Finish off tokenining the number
        self.advance(); // consume the pariod
        self.consume_numbers(); // get the rest of the digits of the (now confirmed floating point
                                // number
        return self.make_token(TokenType::TOKEN_NUMBER); 
    }

    //
    // specific consuming helper functions
    //
   
    fn skipWhitespace(&mut self) -> (){
        // consumes whitespace, which includes comments 
        loop {
            // only a single character look ahead. Comments require two
            let c = match self.peek(){
                Some(val) => val,
                None => return (),
            };
            match *c as char {
                // standard white space skipping
                ' ' | '\r' | '\t' => match self.advance(){Some(_) => continue, None => return ()  },
                '\n' => {
                    self.line += 1;
                    match self.advance(){
                        Some(_) => continue,
                        None => return ()
                    }
                },
                // comment parsing
                '/' => {
                    // need two / to start comment
                    let comment_start = match self.peekNext(){
                        Some(val) => {
                            match *val as char{
                                '/' => true,
                                _ => false,
                            }
                        },
                        None => false
                    };

                    if !comment_start{return ();}
                    // consume all characters until newline or EOF
                    let mut is_newline: bool;
                    loop{
                        is_newline = match self.peek(){
                            Some(val) => match *val as char{
                                '\n' => true,
                                _ => false,
                            },
                            None => false,
                        };
                        if is_newline || self.isAtEnd() {return ();}
                        self.advance();
                    }
                }
                // something that ain't whitespace. We done here
                _ => return (),
            };
        }
    }

    fn consume_numbers(&mut self) -> (){
        // helper function which consumes digit until EOF or non-digit found
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

    // matching identifier functions
    
    fn identifier_type(&mut self) -> TokenType{
        // simple trie implementation to identify reserved keywords and other identifiers
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

    fn checkKeyword(&self, initial_length: usize, remaining_length: usize, expected_remaining_characters: String, ttype: TokenType) -> TokenType{
        // given:
        //      the expected length of the first part of a token
        //      the expected length of the second part of a token
        //      the expected remaining characters of the token
        //      the expected output token
        // 
        // Verify against the source stream if the current token matches this expectation
        //
        // This is used to differentiate reserved words from identifiers (ie. variables, member
        // names etc.). Hence, 'this' return TokenType::TOKEN_THIS, but 'thisssss" return
        // TokenType::TOKEN_IDENTIFIER

        let expected_token_length = initial_length+remaining_length;
        let source_token_length = unsigned_abs_sub(self.current, self.start);
        let keyword_found = 
            (source_token_length == expected_token_length) &&
            // The characters of the 
            self.source.as_bytes()[self.start..self.current] == *expected_remaining_characters.as_bytes();
        if keyword_found{ 
                return ttype;
        }
        return TokenType::TOKEN_IDENTIFIER;
    }

    fn string(&mut self)-> Token{
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

    // token generation helper functions
    fn make_token(&self, a_ttype: TokenType) -> Token{
        Token{ttype: a_ttype, start: self.source[self.start..self.current].to_string(), line: self.line}
    } 

    fn error_token(&self, message: String) -> Token{
        Token{ttype: TokenType::TOKEN_ERROR, start: message.clone(), line: self.line}
    }

    // stream manipulation helper functions
    fn isAtEnd(&self) -> bool{
        // test if at the end of the source. 0-based indexing
        self.source.len() <= self.current
    }

    fn advance(&mut self) -> Option<&u8>{
        // advance current index by 1 and return the previous character
        // can think of this as consuming the current character, then incrementing the pointer
        self.current += 1;
        // remember that String under the hood is just a vector of u8, which can be indexed
        return self.source.as_bytes().get(self.current-1);
    }

    fn peek(&self) -> Option<&u8>{
        // returns the next character in the stream
        return self.source.as_bytes().get(self.current);
    }

    fn Match(&mut self, expected: char) -> bool {
        // a combination of peek and advance which is conditional and doesn't return the consumed
        // character
        if self.isAtEnd() {return false;}
        if self.source.as_bytes()[self.current] != expected as u8 {return false;}
        // Need to discard the successfully matched character!
        self.current += 1;
        return true;
    }

    fn peekNext(&self) -> Option<&u8>{
        // returns the next-next character in the stream
        if self.isAtEnd() {return None}
        return self.source.as_bytes().get(self.current+1);
    }
}

// helper functions that don't need to be tightly coupled with Scanner
pub fn isDigit(character: u8) -> bool{
        let c = character as char;
        return c >= '0' && c <= '9';
}

pub fn isAlpha(character: u8) -> bool {
    // '_' is considered alphabetic since variable names can start with _
        let c = character as char;
        return (c>='a' && c <='z') || (c>='A' && c <='Z') || (c == '_');
}

pub fn unsigned_abs_sub(left: usize, right: usize) -> usize{
    // get the absolute difference between two numbers, irrespective of which is larger
    return left.checked_sub(right).unwrap_or(right-left);
}
