use crate::lexer::lexer::Lexer;
use crate::lexer::token::Token;
use crate::lexer::token::Token::*;
use crate::ast::ast::{Program, Statement, LetStatement, Expression};


type ParsingError = String;
type ParsingResult<T> = Result<T, ParsingError>;

struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        let current = lexer.next_token();
        let peek = lexer.next_token();
        Parser {
            lexer,
            cur_token: current,
            peek_token: peek
        }
    }

    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    fn parse_program(mut self) -> ParsingResult<Program> {
        let mut statements = Vec::new();

        while self.cur_token != Token::EOF {
            let statement = self.parse_statement()?;

            statements.push(statement);
            self.next_token();
        }

        Ok(Program {
            statements
        })
    }


    fn parse_statement(&mut self) -> ParsingResult<Statement> {
        match self.cur_token {
            LET => self.parse_let_statement().map(|s| Statement::Let(s)),
            _ => Err(format!("unknown token: {:?}", self.cur_token))
        }
    }

    fn parse_let_statement(&mut self) -> ParsingResult<LetStatement> {
        let ident_name = self.expect_ident()?;
        self.expect_peek(ASSIGN)?;

        // todo: do not just skip
        while !self.current_token_is(SEMICOLON) {
            self.next_token();
        }

        Ok(LetStatement {
            name: ident_name,
            value: Expression::Identifier("replace me".to_string()),
        })
    }

    fn current_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        &self.peek_token == t
    }

    fn expect_ident(&mut self) -> ParsingResult<String> {
        let result = match &self.peek_token {
            IDENT(name) => Ok(name.clone()),
            other => Err(format!("Expected IDENT but got {:?}", other))
        };

        match result {
            Ok(_) => self.next_token(),
            _ => ()
        }

        result
    }

    fn expect_peek(&mut self, t: Token) -> ParsingResult<()> {
        if self.peek_token_is(&t) {
            self.next_token();
            Ok(())
        } else {
            Err(format!("Expected next token to be {:?} but got {:?}", t, self.peek_token))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast::ast::{Statement};

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("program should be parsable");
        let statements = program.statements;

        assert_eq!(statements.len(), 3, "program.Statements does not contain 3 statements. got {}", statements.len());

        let tests = vec!["x", "y", "foobar"];

        for (id, &test_str) in tests.iter().enumerate() {
            test_let_statement(&statements[id], test_str)
        }
    }

    fn test_let_statement(statement: &Statement, name: &str) {
        match statement {
            Statement::Let(s) => {
                assert_eq!(s.name, name)
            }
        }
    }
}