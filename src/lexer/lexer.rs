use crate::lexer::token::{Token, lookup_ident, TokenType};
use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::token::TokenType::*;

pub struct Lexer<'a> {
    peekable: Peekable<Chars<'a>>
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            peekable: input.chars().peekable()
        }
    }

    fn read_identifier(&mut self, head: char) -> String {
        let mut literal = String::new();
        literal.push(head);
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

    fn read_number(&mut self, head: char) -> String {
        let mut literal = String::new();
        literal.push(head);
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



        let peekable = &mut self.peekable;

        match peekable.next() {
            Some(char) => {

                let create_token = |typ: TokenType| {
                    Token { typ, literal: char.to_string() }
                };

                let token = match char {
                    ';' => create_token(SEMICOLON),
                    '(' => create_token(LPAREN),
                    ')' => create_token(RPAREN),
                    ',' => create_token(COMMA),
                    '+' => create_token(PLUS),
                    '{' => create_token(LBRACE),
                    '}' => create_token(RBRACE),
                    '=' => {
                        match peekable.peek() {
                            Some('=') => {
                                peekable.next();
                                Token { typ: EQ, literal: "==".to_owned() }
                            }
                            _ => {
                                Token { typ: ASSIGN, literal: char.to_string() }
                            }
                        }
                    },
                    '-' => create_token(MINUS),
                    '!' => {
                        match peekable.peek() {
                            Some('=') => {
                                peekable.next();
                                Token { typ: NOT_EQ, literal: "!=".to_owned() }
                            }
                            _ => {
                                Token { typ: BANG, literal: char.to_string() }
                            }
                        }
                    },
                    '/' => create_token(SLASH),
                    '*' => create_token(ASTERISK),
                    '<' => create_token(LT),
                    '>' => create_token(GT),
                    c if c.is_alphabetic() || char == '_' => {
                        let literal = self.read_identifier(c);
                        let typ = lookup_ident(&literal);
                        Token { typ, literal }
                    }
                    d if d.is_ascii_digit() => {
                        let literal = self.read_number(d);
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

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
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
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(BANG, "!".to_owned()),
            TestInput(MINUS, "-".to_owned()),
            TestInput(SLASH, "/".to_owned()),
            TestInput(ASTERISK, "*".to_owned()),
            TestInput(INT, "5".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(INT, "5".to_owned()),
            TestInput(LT, "<".to_owned()),
            TestInput(INT, "10".to_owned()),
            TestInput(GT, ">".to_owned()),
            TestInput(INT, "5".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),

            TestInput(IF, "if".to_owned()),
            TestInput(LPAREN, "(".to_owned()),
            TestInput(INT, "5".to_owned()),
            TestInput(LT, "<".to_owned()),
            TestInput(INT, "10".to_owned()),
            TestInput(RPAREN, ")".to_owned()),
            TestInput(LBRACE, "{".to_owned()),
            TestInput(RETURN, "return".to_owned()),
            TestInput(TRUE, "true".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(RBRACE, "}".to_owned()),
            TestInput(ELSE, "else".to_owned()),
            TestInput(LBRACE, "{".to_owned()),
            TestInput(RETURN, "return".to_owned()),
            TestInput(FALSE, "false".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(RBRACE, "}".to_owned()),

            TestInput(INT, "10".to_owned()),
            TestInput(EQ, "==".to_owned()),
            TestInput(INT, "10".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
            TestInput(INT, "10".to_owned()),
            TestInput(NOT_EQ, "!=".to_owned()),
            TestInput(INT, "9".to_owned()),
            TestInput(SEMICOLON, ";".to_owned()),
        ];

        let mut lexer = Lexer::new(input);


        for test in tests.iter() {
            match lexer.next() {
                Some(token) => {
                    assert_eq!(token.typ, test.0, "test input: {:?}", test);
                    assert_eq!(token.literal, test.1);
                }
                None => {
                    panic!("Input ended unexpectedly")
                }
            }
        }
        assert_eq!(lexer.next().is_none(), true)
    }
}