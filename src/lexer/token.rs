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
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOT_EQ,

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
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN

}

pub fn lookup_ident(keyword: &str) -> TokenType {
    use crate::lexer::token::TokenType::*;
    match keyword {
        "fn" => FUNCTION,
        "let" => LET,
        "true" => TRUE,
        "false" => FALSE,
        "if" => IF,
        "else" => ELSE,
        "return" => RETURN,
        _ => IDENT
    }
}