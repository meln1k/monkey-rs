#[derive(PartialEq, Eq, Debug)]
#[allow(non_camel_case_types)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String),
    INT(String),
    FLOAT(String),
    STRING(String),

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
    LBRACKET,
    RBRACKET,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: i32,
    pub position: i32,
}

pub fn lookup_ident(keyword: String) -> TokenType {
    use crate::lexer::token::TokenType::*;
    match keyword.as_str() {
        "fn" => FUNCTION,
        "let" => LET,
        "true" => TRUE,
        "false" => FALSE,
        "if" => IF,
        "else" => ELSE,
        "return" => RETURN,
        _ => IDENT(keyword),
    }
}
