use crate::ast::ast::Expression::InfixExpression;
use crate::ast::ast::{
    Expression, ExpressionStatement, LetStatement, Program, ReturnStatement, Statement,
};
use crate::lexer::lexer::Lexer;
use crate::lexer::token::Token;
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
                Err(reason) => errors.push(reason),
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
            Token::LET => self.parse_let_statement().map(|s| Statement::Let(s)),
            Token::RETURN => self.parse_return_statement().map(|s| Statement::Return(s)),
            _ => self.parse_expr_statement().map(|s| Statement::Expr(s)),
        }
    }

    fn parse_let_statement(&mut self) -> ParsingResult<LetStatement> {
        let ident_name = self.expect_ident()?;
        self.expect_peek(Token::ASSIGN)?;

        // todo: do not just skip
        while !self.current_token_is(Token::SEMICOLON) {
            self.advance_token();
        }

        Ok(LetStatement {
            name: ident_name,
            value: Expression::Identifier("replace me".to_string()),
        })
    }

    fn parse_return_statement(&mut self) -> ParsingResult<ReturnStatement> {
        self.advance_token();

        while !self.current_token_is(Token::SEMICOLON) {
            self.advance_token();
        }

        Ok(ReturnStatement {
            return_value: Expression::Identifier("not implemented yet".to_owned()),
        })
    }

    fn parse_expr_statement(&mut self) -> ParsingResult<ExpressionStatement> {
        let maybe_expr = self.parse_expression(LOWEST);

        if self.peek_token_is(&Token::SEMICOLON) {
            self.advance_token()
        }

        maybe_expr.map(|expression| ExpressionStatement { expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParsingResult<Expression> {
        let prefix_fn = self.prefix_parse_fn(&self.cur_token);

        let mut left_expr = match prefix_fn {
            Some(func) => func(self),
            None => Err(format!(
                "no prefix parse function for {:?} found",
                self.cur_token
            )),
        }?;

        while !self.current_token_is(Token::SEMICOLON) && precedence < precedences(&self.peek_token)
        {
            match self.infix_parse_fn(&self.peek_token) {
                Some(func) => {
                    self.advance_token();
                    left_expr = func(self, left_expr)?;
                }
                None => return Ok(left_expr),
            }
        }

        Ok(left_expr)
    }

    fn parse_identifier(parser: &mut Parser<'_>) -> ParsingResult<Expression> {
        match &parser.cur_token {
            Token::IDENT(name) => Ok(Expression::Identifier(name.clone())),
            other => Err(format!("expected identifier but got {:?}", other)),
        }
    }

    fn parse_boolean(parser: &mut Parser<'_>) -> ParsingResult<Expression> {
        match &parser.cur_token {
            Token::TRUE => Ok(Expression::Boolean(true)),
            Token::FALSE => Ok(Expression::Boolean(false)),
            other => Err(format!("expected boolean but got {:?}", other)),
        }
    }

    fn parse_integer_literal(parser: &mut Parser<'_>) -> ParsingResult<Expression> {
        match &parser.cur_token {
            Token::INT(value) => value
                .parse()
                .map_err(|_| format!("can't parse value {:?} as int", value))
                .map(Expression::IntegerLiteral),
            other => Err(format!("Expected INT but got {:?}", other)),
        }
    }

    fn parse_prefix_expression(parser: &mut Parser) -> ParsingResult<Expression> {
        let token_str = match &parser.cur_token {
            Token::BANG => Ok("!".to_owned()),
            Token::MINUS => Ok("-".to_owned()),
            other => Err(format!("Expected prefix token but got {:?}", other)),
        };

        parser.advance_token();

        token_str.and_then(|operator| {
            parser
                .parse_expression(PREFIX)
                .map(|expr| Expression::PrefixExpression {
                    operator,
                    expr: Box::new(expr),
                })
        })
    }

    fn parse_infix_expression(parser: &mut Parser, left: Expression) -> ParsingResult<Expression> {
        let operator = match &parser.cur_token {
            Token::PLUS => Ok("+".to_owned()),
            Token::MINUS => Ok("-".to_owned()),
            Token::SLASH => Ok("/".to_owned()),
            Token::ASTERISK => Ok("*".to_owned()),
            Token::EQ => Ok("==".to_owned()),
            Token::NOT_EQ => Ok("!=".to_owned()),
            Token::LT => Ok("<".to_owned()),
            Token::GT => Ok(">".to_owned()),
            other => Err(format!("Expected infix operator but got {:?}", other)),
        }?;

        let precedence = precedences(&parser.cur_token);
        parser.advance_token();
        let right = parser.parse_expression(precedence)?;

        Ok(InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn current_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        self.peek_token == *t
    }

    fn expect_ident(&mut self) -> ParsingResult<String> {
        let result = match &self.peek_token {
            Token::IDENT(name) => Ok(name.clone()),
            other => Err(format!("Expected IDENT but got {:?}", other)),
        };

        match result {
            Ok(_) => self.advance_token(),
            _ => (),
        }

        result
    }

    fn expect_peek(&mut self, t: Token) -> ParsingResult<()> {
        if self.peek_token_is(&t) {
            self.advance_token();
            Ok(())
        } else {
            Err(format!(
                "Expected next token to be {:?} but got {:?}",
                t, self.peek_token
            ))
        }
    }

    fn prefix_parse_fn(&self, token: &Token) -> Option<PrefixParseFn> {
        match token {
            Token::IDENT(_) => Some(Parser::parse_identifier),
            Token::INT(_) => Some(Parser::parse_integer_literal),
            Token::BANG | Token::MINUS => Some(Parser::parse_prefix_expression),
            Token::TRUE | Token::FALSE => Some(Parser::parse_boolean),
            _ => None,
        }
    }

    fn infix_parse_fn(&self, token: &Token) -> Option<InfixParseFn> {
        match token {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK
            | Token::EQ
            | Token::NOT_EQ
            | Token::LT
            | Token::GT => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }
}

fn precedences(token: &Token) -> Precedence {
    use crate::parser::Precedence::*;
    match token {
        Token::EQ => EQUALS,
        Token::NOT_EQ => EQUALS,
        Token::LT => LESSGREATER,
        Token::GT => LESSGREATER,
        Token::PLUS => SUM,
        Token::MINUS => SUM,
        Token::SLASH => PRODUCT,
        Token::ASTERISK => PRODUCT,
        _ => LOWEST,
    }
}

type PrefixParseFn = fn(parser: &mut Parser<'_>) -> ParsingResult<Expression>;
type InfixParseFn = fn(parser: &mut Parser<'_>, left: Expression) -> ParsingResult<Expression>;

#[cfg(test)]
mod tests {
    use crate::ast::ast::Expression::*;
    use crate::ast::ast::{Expression, ExpressionStatement, ReturnStatement, Statement};
    use crate::lexer::lexer::Lexer;
    use crate::parser::Parser;

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

        assert_eq!(
            statements.len(),
            3,
            "program.Statements does not contain 3 statements. got {}",
            statements.len()
        );

        let tests = vec!["x", "y", "foobar"];

        for (id, test_str) in tests.iter().enumerate() {
            test_let_statement(&statements[id], *test_str)
        }
    }

    fn test_let_statement(statement: &Statement, name: &str) {
        match statement {
            Statement::Let(s) => assert_eq!(s.name, name),
            other => panic!("Expected Let but got {:?}", other),
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

        assert_eq!(
            statements.len(),
            3,
            "program.Statements does not contain 3 statements. got {}",
            statements.len()
        );

        let tests = vec!["5", "10", "42"];

        for (id, &test_str) in tests.iter().enumerate() {
            match &statements[id] {
                Statement::Return(ReturnStatement { return_value }) => (),
                other => panic!("Expected Return but got {:?}", other),
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

        assert_eq!(
            statements.len(),
            1,
            "program.Statements does not contain 1 statement."
        );

        let statement = &statements[0];

        match statement {
            Statement::Expr(ExpressionStatement { expression }) => {
                assert_eq!(expression, &Identifier("foobar".to_owned()))
            }
            other => panic!("expected an identifier but got {:?}", other),
        }
    }

    #[test]
    fn test_integer_literal_expr() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("program should be parsable");
        let statements = program.statements;

        assert_eq!(
            statements.len(),
            1,
            "program.Statements does not contain 1 statement."
        );

        let statement = &statements[0];

        match statement {
            Statement::Expr(ExpressionStatement { expression }) => {
                assert_eq!(expression, &IntegerLiteral(5))
            }
            other => panic!("expected an integer literal but got {:?}", other),
        }
    }

    #[test]
    fn test_parsing_prefix_expr() {
        type Input = String;
        type Operator = String;
        type IntValue = i64;

        struct PrefixTest(Input, Operator, IntValue);

        let prefix_tests = vec![
            PrefixTest("!5;".to_owned(), "!".to_owned(), 5),
            PrefixTest("-15;".to_owned(), "-".to_owned(), 15),
        ];

        for PrefixTest(input, op, intval) in prefix_tests {
            let lexer = Lexer::new(&input);
            let program = Parser::new(lexer)
                .parse_program()
                .expect("program should be parsable");
            let statements = program.statements;

            assert_eq!(
                statements.len(),
                1,
                "program.Statements should contain 1 statement."
            );

            match &statements[0] {
                Statement::Expr(ExpressionStatement { expression }) => match expression {
                    Expression::PrefixExpression { operator, expr } => {
                        assert_eq!(*operator, op);
                        test_literal_expression(expr, ExpectedExprValue::Int(intval))
                    }
                    other => panic!("expected PrefixExpression but got {:?}", other),
                },
                other => panic!("expected Expr but got {:?}", other),
            }
        }
    }

    fn test_integer_literal(expr: &Expression, value: i64) {
        match expr {
            Expression::IntegerLiteral(int) => assert_eq!(*int, value),
            other => panic!("expected integer literal but got {:?}", other),
        }
    }

    fn test_identifier(expression: &Expression, value: String) {
        match expression {
            Expression::Identifier(s) => assert_eq!(*s, value),
            other => panic!("expected Identifier but got {:?}", other)
        }
    }

    enum ExpectedExprValue {
        Int(i64),
        String(String),
    }

    fn test_literal_expression(expression: &Expression, value: ExpectedExprValue) {
        match value {
            ExpectedExprValue::Int(int) => test_integer_literal(expression, int),
            ExpectedExprValue::String(string) => test_identifier(expression, string)
        }
    }

    fn test_infix_expression(expr: &Expression, l: ExpectedExprValue, op: String, r: ExpectedExprValue) {
        match expr {
            InfixExpression { left, operator, right } => {
                test_literal_expression(left, l);
                assert_eq!(op, *operator);
                test_literal_expression(right, r);
            }
            other => panic!("InfixExpression Identifier but got {:?}", other)
        }
    }

    #[test]
    fn test_parsing_infix_expr() {
        type Input = String;
        type Operator = String;
        type LeftValue = i64;
        type RightValue = i64;

        struct InfixTest(Input, LeftValue, Operator, RightValue);

        let infix_tests = vec![
            InfixTest("5 + 5;".to_owned(), 5, "+".to_owned(), 5),
            InfixTest("5 - 5;".to_owned(), 5, "-".to_owned(), 5),
            InfixTest("5 * 5;".to_owned(), 5, "*".to_owned(), 5),
            InfixTest("5 / 5;".to_owned(), 5, "/".to_owned(), 5),
            InfixTest("5 > 5;".to_owned(), 5, ">".to_owned(), 5),
            InfixTest("5 < 5;".to_owned(), 5, "<".to_owned(), 5),
            InfixTest("5 == 5;".to_owned(), 5, "==".to_owned(), 5),
            InfixTest("5 != 5;".to_owned(), 5, "!=".to_owned(), 5),
        ];

        for InfixTest(input, left_val, op, right_val) in infix_tests {
            let lexer = Lexer::new(&input);
            let program = Parser::new(lexer)
                .parse_program()
                .expect("program should be parsable");
            let statements = program.statements;

            assert_eq!(
                statements.len(),
                1,
                "program.Statements should contain 1 statement."
            );

            match &statements[0] {
                Statement::Expr(ExpressionStatement { expression }) => {
                    test_infix_expression(expression, ExpectedExprValue::Int(left_val), op, ExpectedExprValue::Int(right_val));
                }
                other => panic!("expected Expr but got {:?}", other),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        type Input = String;
        type Expected = String;
        struct Test(Input, Expected);

        let tests = vec![
            Test("-a * b".to_owned(), "((-a) * b)".to_owned()),
            Test("!-a".to_owned(), "(!(-a))".to_owned()),
            Test("a + b + c".to_owned(), "((a + b) + c)".to_owned()),
            Test("a + b - c".to_owned(), "((a + b) - c)".to_owned()),
            Test("a * b * c".to_owned(), "((a * b) * c)".to_owned()),
            Test("a * b / c".to_owned(), "((a * b) / c)".to_owned()),
            Test("a + b / c".to_owned(), "(a + (b / c))".to_owned()),
            Test(
                "a + b * c + d / e - f".to_owned(),
                "(((a + (b * c)) + (d / e)) - f)".to_owned(),
            ),
            Test("3 + 4; -5 * 5".to_owned(), "(3 + 4)((-5) * 5)".to_owned()),
            Test(
                "5 > 4 == 3 < 4".to_owned(),
                "((5 > 4) == (3 < 4))".to_owned(),
            ),
            Test(
                "5 < 4 != 3 > 4".to_owned(),
                "((5 < 4) != (3 > 4))".to_owned(),
            ),
            Test(
                "3 + 4 * 5 == 3 * 1 + 4 * 5".to_owned(),
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_owned(),
            ),
            Test(
                "3 + 4 * 5 == 3 * 1 + 4 * 5".to_owned(),
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_owned(),
            ),
        ];

        for Test(input, expected) in tests {
            let lexer = Lexer::new(&input);
            let program = Parser::new(lexer)
                .parse_program()
                .expect("program should be parsable");

            let actual = program.to_string();

            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_boolean_expression() {
        type Input = String;
        type Expected = bool;
        struct Test(Input, Expected);

        let tests = vec![
            Test("true;".to_owned(), true),
            Test("false;".to_owned(), false)
        ];

        for Test(input, expected) in tests {
            let lexer = Lexer::new(&input);
            let program = Parser::new(lexer)
                .parse_program()
                .expect("program should be parsable");

            let statements = program.statements;

            assert_eq!(
                statements.len(),
                1,
                "program.Statements should contain 1 statement."
            );

            match &statements[0] {
                Statement::Expr(ExpressionStatement { expression }) => match expression {
                    Boolean(boolean) => assert_eq!(*boolean, expected),
                    other => panic!("expected Boolean but got {:?}", other),

                }
                other => panic!("expected ExpressionStatement but got {:?}", other),
            }
        }
    }
}
