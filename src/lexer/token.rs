use crate::lexer::token::TokenType::{FUNCTION, LET, IDENT};

#[derive(Debug)]
pub struct Token {
    pub typ: TokenType,
    pub literal: String,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TokenType {
    ILLEGAL,
    // EOF,

    // Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    PLUS,

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
}

pub fn lookup_ident(keyword: &str) -> TokenType {
    match keyword {
        "fn" => FUNCTION,
        "let" => LET,
        _ => IDENT
    }
}