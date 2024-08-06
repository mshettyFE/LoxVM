
pub struct Scanner{
    source: String,
    start: usize,
    current: usize,
    line: usize
}

#[derive(Debug)]
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
            _ => return self.error_token("Unexpected character.".to_string()),
        }


    }

    pub fn make_token(&self, a_ttype: TokenType) -> Token{
        Token{ttype: a_ttype, start: self.source[self.start..self.current].to_string(), line: self.line}
    } 

    pub fn error_token(&self, message: String) -> Token{
        Token{ttype: TokenType::TOKEN_ERROR, start: message.clone(), line: self.line}
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
                        loop{
                            let mut is_newline = match self.peek(){
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

