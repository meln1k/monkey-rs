use crate::lexer::token::{Token, lookup_ident};
use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::token::TokenType::*;

struct Lexer<'a> {
    peekable: Peekable<Chars<'a>>
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            peekable: input.chars().peekable()
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut literal = String::new();
        while let Some(&ch) = self.peekable.peek() {
            if ch.is_alphabetic() {
                literal.push(ch);
                self.peekable.next();
            } else {
                break;
            }
        }
        literal
    }

    fn read_number(&mut self) -> String {
        let mut literal = String::new();
        while let Some(&ch) = self.peekable.peek() {
            if ch.is_ascii_digit() {
                literal.push(ch);
                self.peekable.next();
            } else {
                break;
            }
        }
        literal
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peekable.peek() {
            if c.is_whitespace() {
                self.peekable.next();
            } else {
                break;
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        match self.peekable.peek() {
            Some(&char) => {
                let token = match char {
                    '=' => {
                        self.peekable.next();
                        Token { typ: ASSIGN, literal: char.to_string() }
                    }
                    ';' => {
                        self.peekable.next();
                        Token { typ: SEMICOLON, literal: char.to_string() }
                    }
                    '(' => {
                        self.peekable.next();
                        Token { typ: LPAREN, literal: char.to_string() }
                    }
                    ')' => {
                        self.peekable.next();
                        Token { typ: RPAREN, literal: char.to_string() }
                    }
                    ',' => {
                        self.peekable.next();
                        Token { typ: COMMA, literal: char.to_string() }
                    }
                    '+' => {
                        self.peekable.next();
                        Token { typ: PLUS, literal: char.to_string() }
                    }
                    '{' => {
                        self.peekable.next();
                        Token { typ: LBRACE, literal: char.to_string() }
                    }
                    '}' => {
                        self.peekable.next();
                        Token { typ: RBRACE, literal: char.to_string() }
                    }
                    c if c.is_alphabetic() || char == '_' => {
                        let literal = self.read_identifier();
                        let typ = lookup_ident(&literal);
                        Token { typ, literal }
                    }
                    d if d.is_ascii_digit() => {
                        let literal = self.read_number();
                        Token { typ: INT, literal }
                    }
                    _ => Token { typ: ILLEGAL, literal: char.to_string() }
                };
                Some(token)
            }
            None => None
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;

    use crate::lexer::token::TokenType;
    use crate::lexer::token::TokenType::*;
    use crate::lexer::lexer::Lexer;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        ";
        #[derive(Debug)]
        struct TestInput(TokenType, String);

        let tests = vec![
            TestInput(LET, "let".to_owned()),
            TestInput(IDENT, "five".to_owned()),
            TestInput(ASSIGN, "=".to_owned()),
            TestInput(INT, "5".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(LET, "let".to_owned()),
            TestInput(IDENT, "ten".to_owned()),
            TestInput(ASSIGN, "=".to_owned()),
            TestInput(INT, "10".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(LET, "let".to_owned()),
            TestInput(IDENT, "add".to_owned()),
            TestInput(ASSIGN, "=".to_owned()),
            TestInput(FUNCTION, "fn".to_owned()),
            TestInput(LPAREN, "(".to_owned()),
            TestInput(IDENT, "x".to_owned()),
            TestInput(COMMA, ",".to_owned()),
            TestInput(IDENT, "y".to_owned()),
            TestInput(RPAREN, ")".to_owned()),
            TestInput(LBRACE, "{".to_owned()),
            TestInput(IDENT, "x".to_owned()),
            TestInput(PLUS, "+".to_owned()),
            TestInput(IDENT, "y".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(RBRACE, "}".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(LET, "let".to_owned()),
            TestInput(IDENT, "result".to_owned()),
            TestInput(ASSIGN, "=".to_owned()),
            TestInput(IDENT, "add".to_owned()),
            TestInput(LPAREN, "(".to_owned()),
            TestInput(IDENT, "five".to_owned()),
            TestInput(COMMA, ",".to_owned()),
            TestInput(IDENT, "ten".to_owned()),
            TestInput(RPAREN, ")".to_owned()),
            TestInput(SEMICOLON, ";".to_owned())
        ];

        let mut lexer = Lexer::new(input);


        for test in tests.iter() {
            match lexer.next() {
                Some(token) => {
                    assert_eq!(token.typ, test.0);
                    assert_eq!(token.literal, test.1);
                }
                None => {
                    panic!("Input ended unexpectedly")
                }
            }
        }
    }
}