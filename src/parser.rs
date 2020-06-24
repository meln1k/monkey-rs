use crate::lexer::lexer::Lexer;
use crate::lexer::token::Token;
use crate::lexer::token::Token::*;
use crate::ast::ast::{Program, Statement, LetStatement, ReturnStatement, ExpressionStatement, Expression};
use crate::parser::Precedence::{LOWEST, PREFIX};
use std::fmt::format;


type ParsingError = String;
type ParsingErrors = Vec<ParsingError>;
type ParsingResult<T> = Result<T, ParsingError>;

struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
}

#[derive(Ord, PartialOrd, Eq, PartialEq)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
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

    fn advance_token(&mut self) {
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
            self.advance_token();
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
            _ => self.parse_expr_statement().map(|s| Statement::Expr(s))
        }
    }

    fn parse_let_statement(&mut self) -> ParsingResult<LetStatement> {
        let ident_name = self.expect_ident()?;
        self.expect_peek(ASSIGN)?;

        // todo: do not just skip
        while !self.current_token_is(SEMICOLON) {
            self.advance_token();
        }

        Ok(LetStatement {
            name: ident_name,
            value: Expression::Identifier("replace me".to_string()),
        })
    }

    fn parse_return_statement(&mut self) -> ParsingResult<ReturnStatement> {
        self.advance_token();

        while !self.current_token_is(SEMICOLON) {
            self.advance_token();
        }

        Ok(ReturnStatement { return_value: Expression::Identifier("not implemented yet".to_owned()) })
    }

    fn parse_expr_statement(&mut self) -> ParsingResult<ExpressionStatement> {
        let maybe_expr = self.parse_expression(LOWEST);

        if self.peek_token_is(&SEMICOLON) {
            self.advance_token()
        }

        maybe_expr.map(|expression| ExpressionStatement { expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParsingResult<Expression> {
        let prefix_fn = self.prefix_parse_fn(&self.cur_token);

        match prefix_fn {
            Some(func) => func(self),
            None => Err(format!("no prefix parse function for {:?} found", self.cur_token))
        }
    }

    fn parse_identifier(parser: &mut Parser<'_>) -> ParsingResult<Expression> {
        match &parser.cur_token {
            IDENT(name) => Ok(Expression::Identifier(name.clone())),
            other => Err(format!("expected identifier but got {:?}", other))
        }
    }

    fn parse_integer_literal(parser: &mut Parser<'_>) -> ParsingResult<Expression> {
        match &parser.cur_token {
            INT(value) => value.parse().map_err(|_| format!("can't parse value {:?} as int", value)).map(Expression::IntegerLiteral),
            other => Err(format!("Expected INT but got {:?}", other))
        }
    }

    fn parse_prefix_expression(parser: &mut Parser) -> ParsingResult<Expression> {
        let token_str = match &parser.cur_token {
            BANG => Ok("!".to_owned()),
            MINUS => Ok("-".to_owned()),
            other => Err(format!("Expected prefix token but got {:?}", other))
        };

        parser.advance_token();

        token_str.and_then(|operator| parser
            .parse_expression(PREFIX)
            .map(|expr| Expression::PrefixExpression { operator, expr: Box::new(expr) }))
    }

    fn current_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        self.peek_token == *t
    }

    fn expect_ident(&mut self) -> ParsingResult<String> {
        let result = match &self.peek_token {
            IDENT(name) => Ok(name.clone()),
            other => Err(format!("Expected IDENT but got {:?}", other))
        };

        match result {
            Ok(_) => self.advance_token(),
            _ => ()
        }

        result
    }

    fn expect_peek(&mut self, t: Token) -> ParsingResult<()> {
        if self.peek_token_is(&t) {
            self.advance_token();
            Ok(())
        } else {
            Err(format!("Expected next token to be {:?} but got {:?}", t, self.peek_token))
        }
    }

    fn prefix_parse_fn(&self, token: &Token) -> Option<PrefixParseFn> {
        match token {
            IDENT(_) => Some(Parser::parse_identifier),
            INT(_) => Some(Parser::parse_integer_literal),
            BANG => Some(Parser::parse_prefix_expression),
            MINUS => Some(Parser::parse_prefix_expression),
            _ => None
        }
    }
}

type PrefixParseFn = fn(parser: &mut Parser<'_>) -> ParsingResult<Expression>;
type InfixParseFn = fn(parser: &mut Parser<'_>, left: Expression) -> ParsingResult<Expression>;

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast::ast::{Statement, ReturnStatement, ExpressionStatement, Expression};
    use crate::ast::ast::Expression::*;

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

        for (id, test_str) in tests.iter().enumerate() {
            test_let_statement(&statements[id], *test_str)
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
                Statement::Return(ReturnStatement { return_value }) => {
                    ()
                }
                other => {
                    panic!("Expected Return but got {:?}", other)
                }
            }
        }
    }

    #[test]
    fn test_identifier_expr() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("program should be parsable");
        let statements = program.statements;

        assert_eq!(statements.len(), 1, "program.Statements does not contain 1 statement.");

        let statement = &statements[0];

        match statement {
            Statement::Expr(ExpressionStatement { expression }) => {
                assert_eq!(expression, &Identifier("foobar".to_owned()))
            }
            other => panic!("expected an identifier but got {:?}", other)
        }
    }

    #[test]
    fn test_integer_literal_expr() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("program should be parsable");
        let statements = program.statements;

        assert_eq!(statements.len(), 1, "program.Statements does not contain 1 statement.");

        let statement = &statements[0];

        match statement {
            Statement::Expr(ExpressionStatement { expression }) => {
                assert_eq!(expression, &IntegerLiteral(5))
            }
            other => panic!("expected an integer literal but got {:?}", other)
        }
    }

    #[test]
    fn test_parsing_prefix_expr() {
        type Input = String;
        type Operator = String;
        type IntValue = i64;

        struct PrefixTests(Input, Operator, IntValue);

        let prefix_tests = vec![
            PrefixTests("!5;".to_owned(), "!".to_owned(), 5),
            PrefixTests("-15;".to_owned(), "-".to_owned(), 15)
        ];

        for test in prefix_tests {
            let lexer = Lexer::new(&test.0);
            let program = Parser::new(lexer).parse_program().expect("program should be parsable");
            let statements = program.statements;

            assert_eq!(statements.len(), 1, "program.Statements should contain 1 statement.");

            match &statements[0] {
                Statement::Expr(ExpressionStatement { expression }) => match expression {
                    Expression::PrefixExpression { operator, expr } => {
                        assert_eq!(*operator, test.1);
                        test_integer_literal(expr, test.2)
                    }
                    other => panic!("expected PrefixExpression but got {:?}", other)
                },
                other => panic!("expected Expr but got {:?}", other)
            }
        }
    }

    fn test_integer_literal(expr: &Expression, value: i64) {
        match expr {
            Expression::IntegerLiteral(int) => {
                assert_eq!(*int, value)
            }
            other => panic!("expected integer literal but got {:?}", other)
        }
    }
}