use crate::lexer::token::TokenType::*;
use crate::lexer::token::{lookup_ident, Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    _peekable: Peekable<Chars<'a>>,
    pub current_line: i32,
    pub current_position: i32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            _peekable: input.chars().peekable(),
            current_line: 1,
            current_position: 0,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.current_position += 1;
        self._peekable.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self._peekable.peek()
    }

    fn token(&mut self, token_type: TokenType) -> Token {
        self.token_pos(token_type, self.current_position)
    }

    fn token_pos(&mut self, token_type: TokenType, position: i32) -> Token {
        Token {
            token_type,
            line: self.current_line,
            position: position,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.next_char() {
            Some(char) => match char {
                ';' => self.token(SEMICOLON),
                '(' => self.token(LPAREN),
                ')' => self.token(RPAREN),
                ',' => self.token(COMMA),
                '+' => self.token(PLUS),
                '{' => self.token(LBRACE),
                '}' => self.token(RBRACE),
                '=' => match self.peek_char() {
                    Some('=') => {
                        let pos = self.current_position;
                        self.next_char();
                        self.token_pos(EQ, pos)
                    }
                    _ => self.token(ASSIGN),
                },
                '-' => self.token(MINUS),
                '!' => match self.peek_char() {
                    Some('=') => {
                        let pos = self.current_position;
                        self.next_char();
                        self.token_pos(NOT_EQ, pos)
                    }
                    _ => self.token(BANG),
                },
                '/' => self.token(SLASH),
                '*' => self.token(ASTERISK),
                '<' => self.token(LT),
                '>' => self.token(GT),
                c if c.is_alphabetic() || char == '_' => {
                    let pos = self.current_position;

                    let literal = self.read_identifier(c);
                    let token_type = lookup_ident(literal);
                    self.token_pos(token_type, pos)
                }
                d if d.is_ascii_digit() => {
                    let pos = self.current_position;
                    let (literal, is_float) = self.read_number(d);
                    if is_float {
                        self.token_pos(FLOAT(literal), pos)
                    } else {
                        self.token_pos(INT(literal), pos)
                    }
                }
                _ => self.token(ILLEGAL),
            },
            None => self.token(EOF),
        }
    }

    fn read_identifier(&mut self, head: char) -> String {
        let mut literal = String::new();
        literal.push(head);
        while let Some(&ch) = self.peek_char() {
            if ch.is_alphabetic() {
                literal.push(ch);
                self.next_char();
            } else {
                break;
            }
        }
        literal
    }

    fn read_number(&mut self, head: char) -> (String, bool) {
        let mut literal = String::new();
        let mut fraction_seen = false;
        literal.push(head);
        while let Some(&ch) = self.peek_char() {
            match ch {
                c if c.is_ascii_digit() => {
                    literal.push(ch);
                    self.next_char();
                }
                '.' if !fraction_seen => {
                    literal.push(ch);
                    fraction_seen = true;
                    self.next_char();
                }
                _ => break,
            }
        }
        (literal, fraction_seen)
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek_char() {
            match c {
                '\r' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('\n') => self.start_newline(),
                        _ => break,
                    }
                }
                '\n' => self.start_newline(),
                c if c.is_whitespace() => {
                    self.next_char();
                }
                _ => break,
            }
        }
    }

    fn start_newline(&mut self) {
        self.current_line += 1;
        self.current_position = 0;
        self._peekable.next();
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let next_token = self.next_token();
        match &next_token.token_type {
            EOF => None,
            other => Some(next_token),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::lexer::lexer::Lexer;
    use crate::lexer::token::TokenType;
    use crate::lexer::token::TokenType::*;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;
        let pi = 3.14;

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        ";

        #[derive(Debug)]
        struct TestInput(TokenType);

        let tests = vec![
            TestInput(LET),
            TestInput(IDENT("five".to_owned())),
            TestInput(ASSIGN),
            TestInput(INT("5".to_owned())),
            TestInput(SEMICOLON),
            TestInput(LET),
            TestInput(IDENT("ten".to_owned())),
            TestInput(ASSIGN),
            TestInput(INT("10".to_owned())),
            TestInput(SEMICOLON),
            TestInput(LET),
            TestInput(IDENT("pi".to_owned())),
            TestInput(ASSIGN),
            TestInput(FLOAT("3.14".to_owned())),
            TestInput(SEMICOLON),
            TestInput(LET),
            TestInput(IDENT("add".to_owned())),
            TestInput(ASSIGN),
            TestInput(FUNCTION),
            TestInput(LPAREN),
            TestInput(IDENT("x".to_owned())),
            TestInput(COMMA),
            TestInput(IDENT("y".to_owned())),
            TestInput(RPAREN),
            TestInput(LBRACE),
            TestInput(IDENT("x".to_owned())),
            TestInput(PLUS),
            TestInput(IDENT("y".to_owned())),
            TestInput(SEMICOLON),
            TestInput(RBRACE),
            TestInput(SEMICOLON),
            TestInput(LET),
            TestInput(IDENT("result".to_owned())),
            TestInput(ASSIGN),
            TestInput(IDENT("add".to_owned())),
            TestInput(LPAREN),
            TestInput(IDENT("five".to_owned())),
            TestInput(COMMA),
            TestInput(IDENT("ten".to_owned())),
            TestInput(RPAREN),
            TestInput(SEMICOLON),
            TestInput(BANG),
            TestInput(MINUS),
            TestInput(SLASH),
            TestInput(ASTERISK),
            TestInput(INT("5".to_owned())),
            TestInput(SEMICOLON),
            TestInput(INT("5".to_owned())),
            TestInput(LT),
            TestInput(INT("10".to_owned())),
            TestInput(GT),
            TestInput(INT("5".to_owned())),
            TestInput(SEMICOLON),
            TestInput(IF),
            TestInput(LPAREN),
            TestInput(INT("5".to_owned())),
            TestInput(LT),
            TestInput(INT("10".to_owned())),
            TestInput(RPAREN),
            TestInput(LBRACE),
            TestInput(RETURN),
            TestInput(TRUE),
            TestInput(SEMICOLON),
            TestInput(RBRACE),
            TestInput(ELSE),
            TestInput(LBRACE),
            TestInput(RETURN),
            TestInput(FALSE),
            TestInput(SEMICOLON),
            TestInput(RBRACE),
            TestInput(INT("10".to_owned())),
            TestInput(EQ),
            TestInput(INT("10".to_owned())),
            TestInput(SEMICOLON),
            TestInput(INT("10".to_owned())),
            TestInput(NOT_EQ),
            TestInput(INT("9".to_owned())),
            TestInput(SEMICOLON),
            TestInput(EOF),
        ];

        let mut lexer = Lexer::new(input);

        for test in tests.iter() {
            let token = lexer.next_token();
            assert_eq!(
                token.token_type, test.0,
                "test input: {:?}, token: {:?}",
                test, token
            )
        }
    }

    #[test]
    fn test_token_position() {
        let input = "let five = 5;
let ten = 10;";

        type Line = i32;
        type Position = i32;

        #[derive(Debug)]
        struct TestInput(TokenType, Position, Line);

        let tests = vec![
            TestInput(LET, 1, 1),
            TestInput(IDENT("five".to_owned()), 1, 5),
            TestInput(ASSIGN, 1, 10),
            TestInput(INT("5".to_owned()), 1, 12),
            TestInput(SEMICOLON, 1, 13),
            TestInput(LET, 2, 1),
            TestInput(IDENT("ten".to_owned()), 2, 5),
            TestInput(ASSIGN, 2, 9),
            TestInput(INT("10".to_owned()), 2, 11),
            TestInput(SEMICOLON, 2, 13),
        ];

        let mut lexer = Lexer::new(input);

        for test in tests.iter() {
            let token = lexer.next_token();
            assert_eq!(
                token.token_type, test.0,
                "test input: {:?}, token: {:?}",
                test, token
            );
            assert_eq!(
                token.line, test.1,
                "test input: {:?}, token: {:?}",
                test, token
            );
            assert_eq!(
                token.position, test.2,
                "test input: {:?}, token: {:?}",
                test, token
            );
        }
    }
}
