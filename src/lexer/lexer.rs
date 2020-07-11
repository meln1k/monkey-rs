use crate::lexer::token::Token::*;
use crate::lexer::token::{lookup_ident, Token};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    peekable: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            peekable: input.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let peekable = &mut self.peekable;

        match peekable.next() {
            Some(char) => match char {
                ';' => SEMICOLON,
                '(' => LPAREN,
                ')' => RPAREN,
                ',' => COMMA,
                '+' => PLUS,
                '{' => LBRACE,
                '}' => RBRACE,
                '=' => match peekable.peek() {
                    Some('=') => {
                        peekable.next();
                        EQ
                    }
                    _ => ASSIGN,
                },
                '-' => MINUS,
                '!' => match peekable.peek() {
                    Some('=') => {
                        peekable.next();
                        NOT_EQ
                    }
                    _ => BANG,
                },
                '/' => SLASH,
                '*' => ASTERISK,
                '<' => LT,
                '>' => GT,
                c if c.is_alphabetic() || char == '_' => {
                    let literal = self.read_identifier(c);
                    lookup_ident(literal)
                }
                d if d.is_ascii_digit() => {
                    let (literal, is_float) = self.read_number(d);
                    if is_float {
                        FLOAT(literal)
                    } else {
                        INT(literal)
                    }
                }
                _ => ILLEGAL,
            },
            None => EOF,
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

    fn read_number(&mut self, head: char) -> (String, bool) {
        let mut literal = String::new();
        let mut fraction_seen = false;
        literal.push(head);
        while let Some(&ch) = self.peekable.peek() {
            match ch {
                c if c.is_ascii_digit() => {
                    literal.push(ch);
                    self.peekable.next();
                }
                '.' if !fraction_seen => {
                    literal.push(ch);
                    fraction_seen = true;
                    self.peekable.next();
                }
                _ => break,
            }
        }
        (literal, fraction_seen)
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
        match self.next_token() {
            EOF => None,
            other => Some(other),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::lexer::lexer::Lexer;
    use crate::lexer::token::Token;
    use crate::lexer::token::Token::*;

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
        struct TestInput(Token);

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
            assert_eq!(token, test.0, "test input: {:?}", test)
        }
    }
}
