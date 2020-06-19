use crate::lexer::lexer::Lexer;
use crate::lexer::token::Token;
use crate::lexer::token::Token::*;
use crate::ast::ast::{Program, Statement, LetStatement, Expression, ReturnStatement};
use crate::ast::ast::Expression::Identifier;


type ParsingError = String;
type ParsingErrors = Vec<ParsingError>;
type ParsingResult<T> = Result<T, ParsingError>;

struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        let current = lexer.next_token();
        let peek = lexer.next_token();
        Parser {
            lexer,
            cur_token: current,
            peek_token: peek,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    fn parse_program(mut self) -> Result<Program, ParsingErrors> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while self.cur_token != Token::EOF {
            match self.parse_statement() {
                Ok(s) => statements.push(s),
                Err(reason) => errors.push(reason)
            }
            self.next_token();
        }

        if errors.is_empty() {
            Ok(Program { statements })
        } else {
            Err(errors)
        }
    }


    fn parse_statement(&mut self) -> ParsingResult<Statement> {
        match self.cur_token {
            LET => self.parse_let_statement().map(|s| Statement::Let(s)),
            RETURN => self.parse_return_statement().map(|s| Statement::Return(s)),
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

    fn parse_return_statement(&mut self) -> ParsingResult<ReturnStatement> {

        self.next_token();

        while !self.current_token_is(SEMICOLON) {
            self.next_token();
        }


        Ok(ReturnStatement{return_value: Identifier("not implemented yet".to_owned())})

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
    use crate::ast::ast::{Statement, ReturnStatement};

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
            other => panic!("Expected Let but got {:?}", other)
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 42;";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("program should be parsable");
        let statements = program.statements;

        assert_eq!(statements.len(), 3, "program.Statements does not contain 3 statements. got {}", statements.len());

        let tests = vec!["5", "10", "42"];

        for (id, &test_str) in tests.iter().enumerate() {
            match &statements[id] {
                Statement::Return(ReturnStatement{return_value}) => {
                    ()
                },
                other => {
                    panic!("Expected Return but got {:?}", other)
                }
            }
        }
    }
}