use crate::ast::Expression::{
    CallExpression, FunctionLiteral, IfExpression, IndexExpression, InfixExpression,
};
use crate::ast::{
    BlockStatement, Expression, ExpressionStatement, Identifier, InfixOperator, LetStatement,
    PrefixOperator, Program, ReturnStatement, Statement,
};
use crate::lexer::lexer::Lexer;
use crate::lexer::token::{Token, TokenType};
use crate::parser::Precedence::{LOWEST, PREFIX};
use core::fmt;
use std::fmt::{Display, Formatter};
use std::process::exit;

#[derive(Debug)]
pub struct ParsingError {
    message: String,
    line: i32,
    positon: i32,
}

type ParsingErrors = Vec<ParsingError>;
type ParsingResult<T> = Result<T, ParsingError>;

pub struct Parser<'a> {
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
    INDEX,
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

    fn error<T>(&self, message: String) -> ParsingResult<T> {
        Err(ParsingError {
            message,
            line: self.cur_token.line,
            positon: self.cur_token.position,
        })
    }

    pub fn parse_program(mut self) -> Result<Program, ParsingErrors> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while self.cur_token.token_type != TokenType::EOF {
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
        match self.cur_token.token_type {
            TokenType::LET => self.parse_let_statement().map(|s| Statement::Let(s)),
            TokenType::RETURN => self.parse_return_statement().map(|s| Statement::Return(s)),
            _ => self.parse_expr_statement().map(|s| Statement::Expr(s)),
        }
    }

    fn parse_let_statement(&mut self) -> ParsingResult<LetStatement> {
        let name = self.expect_ident()?;

        self.expect_peek(TokenType::ASSIGN)?;

        self.advance_token();

        let value = self.parse_expression(LOWEST)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.advance_token();
        }

        Ok(LetStatement { name, value })
    }

    fn parse_return_statement(&mut self) -> ParsingResult<ReturnStatement> {
        self.advance_token();

        let return_value = self.parse_expression(LOWEST)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.advance_token();
        }

        Ok(ReturnStatement { return_value })
    }

    fn parse_expr_statement(&mut self) -> ParsingResult<ExpressionStatement> {
        let maybe_expr = self.parse_expression(LOWEST);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.advance_token()
        }

        maybe_expr.map(|expression| ExpressionStatement { expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParsingResult<Expression> {
        let mut left_expr = match self.cur_token.token_type {
            TokenType::IDENT(_) => self.parse_identifier(),
            TokenType::INT(_) => self.parse_integer_literal(),
            TokenType::FLOAT(_) => self.parse_float_literal(),
            TokenType::BANG | TokenType::MINUS => self.parse_prefix_expression(),
            TokenType::TRUE | TokenType::FALSE => self.parse_boolean(),
            TokenType::LPAREN => self.parse_grouped_expression(),
            TokenType::IF => self.parse_if_expression(),
            TokenType::FUNCTION => self.parse_function_literal(),
            TokenType::STRING(_) => self.parse_string_literal(),
            TokenType::LBRACKET => self.parse_array_literal(),
            _ => self.error(format!(
                "no prefix parse function for {:?} found",
                self.cur_token
            )),
        }?;

        while !self.current_token_is(TokenType::SEMICOLON)
            && precedence < precedences(&self.peek_token)
        {
            match &self.peek_token.token_type {
                TokenType::PLUS
                | TokenType::MINUS
                | TokenType::SLASH
                | TokenType::ASTERISK
                | TokenType::EQ
                | TokenType::NOT_EQ
                | TokenType::LT
                | TokenType::GT => {
                    self.advance_token();
                    left_expr = self.parse_infix_expression(left_expr)?;
                }
                TokenType::LPAREN => {
                    self.advance_token();
                    left_expr = self.parse_call_expression(left_expr)?;
                }
                TokenType::LBRACKET => {
                    self.advance_token();
                    left_expr = self.parse_index_expression(left_expr)?;
                }
                _ => return Ok(left_expr),
            }
        }

        Ok(left_expr)
    }

    fn parse_identifier(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token.token_type {
            TokenType::IDENT(name) => Ok(Expression::Ident(Identifier(name.clone()))),
            other => self.error(format!("expected identifier but got {:?}", other)),
        }
    }

    fn parse_boolean(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token.token_type {
            TokenType::TRUE => Ok(Expression::Boolean(true)),
            TokenType::FALSE => Ok(Expression::Boolean(false)),
            other => self.error(format!("expected boolean but got {:?}", other)),
        }
    }

    fn parse_integer_literal(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token.token_type {
            TokenType::INT(value) => match value.parse() {
                Ok(int) => Ok(Expression::IntegerLiteral(int)),
                Err(err) => self.error(format!("can't parse value {:?} as int: {}", value, err)),
            },
            other => self.error(format!("Expected INT but got {:?}", other)),
        }
    }

    fn parse_float_literal(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token.token_type {
            TokenType::FLOAT(value) => match value.parse() {
                Ok(int) => Ok(Expression::FloatLiteral(int)),
                Err(err) => self.error(format!("can't parse value {:?} as float: {}", value, err)),
            },
            other => self.error(format!("Expected FLOAT but got {:?}", other)),
        }
    }

    fn parse_string_literal(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token.token_type {
            TokenType::STRING(value) => Ok(Expression::StringLiteral {
                value: value.clone(),
            }),
            other => self.error(format!("Expected STRING but got {:?}", other)),
        }
    }

    fn parse_array_literal(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token.token_type {
            TokenType::LBRACKET => {
                let elements = self.parse_expression_list(TokenType::RBRACKET)?;
                Ok(Expression::ArrayLiteral { elements })
            }
            other => self.error(format!("Expected LBRACKET but got {:?}", other)),
        }
    }

    fn parse_grouped_expression(&mut self) -> ParsingResult<Expression> {
        self.advance_token();

        let expr = self.parse_expression(LOWEST)?;

        self.expect_peek(TokenType::RPAREN)?;

        Ok(expr)
    }

    fn parse_expression_list(&mut self, end: TokenType) -> ParsingResult<Vec<Expression>> {
        let mut expressions = Vec::new();

        if self.peek_token_is(&end) {
            self.advance_token();
            return Ok(expressions);
        }

        self.advance_token();

        expressions.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token_is(&TokenType::COMMA) {
            self.advance_token();
            self.advance_token();
            expressions.push(self.parse_expression(Precedence::LOWEST)?);
        }

        self.expect_peek(end)?;

        Ok(expressions)
    }

    fn parse_prefix_expression(&mut self) -> ParsingResult<Expression> {
        let operator = match &self.cur_token.token_type {
            TokenType::BANG => Ok(PrefixOperator::BANG),
            TokenType::MINUS => Ok(PrefixOperator::MINUS),
            other => self.error(format!("Expected prefix token but got {:?}", other)),
        }?;

        self.advance_token();

        self.parse_expression(PREFIX)
            .map(|expr| Expression::PrefixExpression {
                operator,
                expr: Box::new(expr),
            })
    }

    fn parse_if_expression(&mut self) -> ParsingResult<Expression> {
        self.expect_peek(TokenType::LPAREN)?;
        self.advance_token();
        let condition = Box::new(self.parse_expression(LOWEST)?);
        self.expect_peek(TokenType::RPAREN)?;
        self.expect_peek(TokenType::LBRACE)?;
        let consequence = Box::new(self.parse_block_statement()?);

        let alternative = if self.peek_token_is(&TokenType::ELSE) {
            self.advance_token();
            self.expect_peek(TokenType::LBRACE)?;
            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };

        Ok(IfExpression {
            condition,
            consequence,
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> ParsingResult<BlockStatement> {
        self.advance_token();
        let mut statements: Vec<Statement> = Vec::new();

        while !self.current_token_is(TokenType::RBRACE) && !self.current_token_is(TokenType::EOF) {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.advance_token();
        }
        Ok(BlockStatement { statements })
    }

    fn parse_function_literal(&mut self) -> ParsingResult<Expression> {
        self.expect_peek(TokenType::LPAREN)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenType::LBRACE)?;

        let body = self.parse_block_statement()?;

        Ok(FunctionLiteral { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> ParsingResult<Vec<Identifier>> {
        let mut identifiers: Vec<Identifier> = Vec::new();

        if self.peek_token_is(&TokenType::RPAREN) {
            self.advance_token();
            return Ok(identifiers);
        }

        self.advance_token();

        let ident = match &self.cur_token.token_type {
            TokenType::IDENT(value) => Ok(Identifier(value.clone())),
            other => self.error(format!("expected identifier but got {:?}", other)),
        }?;

        identifiers.push(ident);

        while self.peek_token_is(&TokenType::COMMA) {
            self.advance_token();
            self.advance_token();

            let ident = match &self.cur_token.token_type {
                TokenType::IDENT(value) => Ok(Identifier(value.clone())),
                other => self.error(format!("expected identifier but got {:?}", other)),
            }?;

            identifiers.push(ident);
        }

        self.expect_peek(TokenType::RPAREN)?;

        Ok(identifiers)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> ParsingResult<Expression> {
        let operator = match &self.cur_token.token_type {
            TokenType::PLUS => Ok(InfixOperator::PLUS),
            TokenType::MINUS => Ok(InfixOperator::MINUS),
            TokenType::SLASH => Ok(InfixOperator::SLASH),
            TokenType::ASTERISK => Ok(InfixOperator::ASTERISK),
            TokenType::EQ => Ok(InfixOperator::EQ),
            TokenType::NOT_EQ => Ok(InfixOperator::NOT_EQ),
            TokenType::LT => Ok(InfixOperator::LT),
            TokenType::GT => Ok(InfixOperator::GT),
            other => self.error(format!("Expected infix operator but got {:?}", other)),
        }?;

        let precedence = precedences(&self.cur_token);
        self.advance_token();
        let right = self.parse_expression(precedence)?;

        Ok(InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn parse_call_expression(&mut self, func: Expression) -> ParsingResult<Expression> {
        let arguments = self.parse_expression_list(TokenType::RPAREN)?;
        let function = Box::new(func);
        Ok(CallExpression {
            function,
            arguments,
        })
    }

    fn parse_index_expression(&mut self, left: Expression) -> ParsingResult<Expression> {
        self.advance_token();

        let index = Box::new(self.parse_expression(Precedence::LOWEST)?);

        self.expect_peek(TokenType::RBRACKET)?;

        Ok(IndexExpression {
            left: Box::new(left),
            index,
        })
    }

    fn current_token_is(&self, t: TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token.token_type == *t
    }

    fn expect_ident(&mut self) -> ParsingResult<Identifier> {
        let result = match &self.peek_token.token_type {
            TokenType::IDENT(name) => Ok(Identifier(name.clone())),
            other => self.error(format!("Expected IDENT but got {:?}", other)),
        }?;

        self.advance_token();

        Ok(result)
    }

    fn expect_peek(&mut self, t: TokenType) -> ParsingResult<()> {
        if self.peek_token_is(&t) {
            Ok(self.advance_token())
        } else {
            self.error(format!(
                "Expected next token to be {:?} but got {:?}",
                t, self.peek_token
            ))
        }
    }
}

fn precedences(token: &Token) -> Precedence {
    use crate::parser::Precedence::*;
    match token.token_type {
        TokenType::EQ => EQUALS,
        TokenType::NOT_EQ => EQUALS,
        TokenType::LT => LESSGREATER,
        TokenType::GT => LESSGREATER,
        TokenType::PLUS => SUM,
        TokenType::MINUS => SUM,
        TokenType::SLASH => PRODUCT,
        TokenType::ASTERISK => PRODUCT,
        TokenType::LPAREN => CALL,
        TokenType::LBRACKET => INDEX,
        _ => LOWEST,
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[{}:{}] {}", self.line, self.positon, self.message)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Expression::*;
    use crate::ast::{
        Expression, ExpressionStatement, Identifier, InfixOperator, LetStatement, PrefixOperator,
        ReturnStatement, Statement,
    };
    use crate::lexer::lexer::Lexer;
    use crate::parser::tests::ExpectedExprValue::{Bool, Int, Str};
    use crate::parser::Parser;

    #[test]
    fn test_let_statements() {
        struct Test(String, Identifier, ExpectedExprValue);

        let tests = vec![
            Test("let x = 5;".to_owned(), Identifier("x".to_owned()), Int(5)),
            Test(
                "let y = true;".to_owned(),
                Identifier("y".to_owned()),
                Bool(true),
            ),
            Test(
                "let foobar = y;".to_owned(),
                Identifier("foobar".to_owned()),
                Str("y".to_owned()),
            ),
        ];

        for Test(input, expected_identifier, expected_value) in tests {
            let lexer = Lexer::new(&input);
            let parser = Parser::new(lexer);

            let program = parser.parse_program().expect("program should be parsable");

            let statements = program.statements;

            assert_eq!(
                statements.len(),
                1,
                "program.Statements does not contain 1 statement."
            );

            let statement = &statements[0];

            test_let_statement(statement, expected_identifier);

            match statement {
                Statement::Let(LetStatement { name: _, value }) => {
                    test_literal_expression(value, expected_value);
                }
                other => panic!("Expected Let but got {:?}", other),
            }
        }
    }

    fn test_let_statement(statement: &Statement, name: Identifier) {
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

        let tests = vec![5, 10, 42];

        for (id, expected) in tests.iter().enumerate() {
            match &statements[id] {
                Statement::Return(ReturnStatement { return_value }) => {
                    test_literal_expression(return_value, Int(*expected))
                }
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
                assert_eq!(expression, &Ident(Identifier("foobar".to_owned())))
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
    fn test_float_literal_expr() {
        let input = "42.42;";

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
                assert_eq!(expression, &FloatLiteral(42.42))
            }
            other => panic!("expected an integer literal but got {:?}", other),
        }
    }

    #[test]
    fn test_parsing_prefix_expr() {
        type Input = String;
        type Value = ExpectedExprValue;

        struct PrefixTest(Input, PrefixOperator, Value);

        use ExpectedExprValue::*;

        let prefix_tests = vec![
            PrefixTest("!5;".to_owned(), PrefixOperator::BANG, Int(5)),
            PrefixTest("-15;".to_owned(), PrefixOperator::MINUS, Int(15)),
            PrefixTest("!true;".to_owned(), PrefixOperator::BANG, Bool(true)),
            PrefixTest("!false;".to_owned(), PrefixOperator::BANG, Bool(false)),
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
                        test_literal_expression(expr, intval)
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
            Expression::Ident(s) => assert_eq!(*s, Identifier(value)),
            other => panic!("expected Identifier but got {:?}", other),
        }
    }

    fn test_boolean_literal(expr: &Expression, value: bool) {
        match expr {
            Expression::Boolean(b) => assert_eq!(*b, value),
            other => panic!("expected Boolean but got {:?}", other),
        }
    }

    enum ExpectedExprValue {
        Int(i64),
        Str(String),
        Bool(bool),
    }

    fn test_literal_expression(expression: &Expression, value: ExpectedExprValue) {
        match value {
            ExpectedExprValue::Int(int) => test_integer_literal(expression, int),
            ExpectedExprValue::Str(string) => test_identifier(expression, string),
            ExpectedExprValue::Bool(b) => test_boolean_literal(expression, b),
        }
    }

    fn test_infix_expression(
        expr: &Expression,
        l: ExpectedExprValue,
        op: InfixOperator,
        r: ExpectedExprValue,
    ) {
        match expr {
            InfixExpression {
                left,
                operator,
                right,
            } => {
                test_literal_expression(left, l);
                assert_eq!(op, *operator);
                test_literal_expression(right, r);
            }
            other => panic!("InfixExpression Identifier but got {:?}", other),
        }
    }

    #[test]
    fn test_parsing_infix_expr() {
        type Input = String;
        type LeftValue = ExpectedExprValue;
        type RightValue = ExpectedExprValue;

        use ExpectedExprValue::*;
        use InfixOperator::*;

        struct InfixTest(Input, LeftValue, InfixOperator, RightValue);

        let infix_tests = vec![
            InfixTest("5 + 5;".to_owned(), Int(5), PLUS, Int(5)),
            InfixTest("5 - 5;".to_owned(), Int(5), MINUS, Int(5)),
            InfixTest("5 * 5;".to_owned(), Int(5), ASTERISK, Int(5)),
            InfixTest("5 / 5;".to_owned(), Int(5), SLASH, Int(5)),
            InfixTest("5 > 5;".to_owned(), Int(5), GT, Int(5)),
            InfixTest("5 < 5;".to_owned(), Int(5), LT, Int(5)),
            InfixTest("5 == 5;".to_owned(), Int(5), EQ, Int(5)),
            InfixTest("5 != 5;".to_owned(), Int(5), NOT_EQ, Int(5)),
            InfixTest("true == true".to_owned(), Bool(true), EQ, Bool(true)),
            InfixTest("true != false".to_owned(), Bool(true), NOT_EQ, Bool(false)),
            InfixTest("false == false".to_owned(), Bool(false), EQ, Bool(false)),
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
                    test_infix_expression(expression, left_val, op, right_val);
                }
                other => panic!("expected Expr but got {:?}", other),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct Test<'a>(&'a str, &'a str);

        let tests = vec![
            Test("-a * b", "((-a) * b)"),
            Test("!-a", "(!(-a))"),
            Test("a + b + c", "((a + b) + c)"),
            Test("a + b - c", "((a + b) - c)"),
            Test("a * b * c", "((a * b) * c)"),
            Test("a * b / c", "((a * b) / c)"),
            Test("a + b / c", "(a + (b / c))"),
            Test("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            Test("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            Test("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            Test("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            Test(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            Test(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            Test("true", "true"),
            Test("false", "false"),
            Test("3 > 5 == false", "((3 > 5) == false)"),
            Test("3 < 5 == true", "((3 < 5) == true)"),
            Test("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            Test("(5 + 5) * 2", "((5 + 5) * 2)"),
            Test("2 / (5 + 5)", "(2 / (5 + 5))"),
            Test("-(5 + 5)", "(-(5 + 5))"),
            Test("!(true == true)", "(!(true == true))"),
            Test("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            Test(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            Test(
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            Test(
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            Test(
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for Test(input, expected) in tests {
            let lexer = Lexer::new(input);
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
            Test("false;".to_owned(), false),
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
                },
                other => panic!("expected ExpressionStatement but got {:?}", other),
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input);
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
                IfExpression {
                    condition,
                    consequence,
                    alternative,
                } => {
                    test_infix_expression(
                        condition,
                        Str("x".to_owned()),
                        InfixOperator::LT,
                        Str("y".to_owned()),
                    );
                    assert_eq!(consequence.statements.len(), 1);
                    match &consequence.statements[0] {
                        Statement::Expr(ExpressionStatement { expression }) => {
                            test_identifier(expression, "x".to_string())
                        }
                        other => panic!("expected ExpressionStatement but got {:?}", other),
                    };
                    assert_eq!(*alternative, None)
                }
                other => panic!("expected IfExpression but got {:?}", other),
            },
            other => panic!("expected ExpressionStatement but got {:?}", other),
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input);
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
                IfExpression {
                    condition,
                    consequence,
                    alternative,
                } => {
                    test_infix_expression(
                        condition,
                        Str("x".to_owned()),
                        InfixOperator::LT,
                        Str("y".to_owned()),
                    );
                    assert_eq!(consequence.statements.len(), 1);
                    match &consequence.statements[0] {
                        Statement::Expr(ExpressionStatement { expression }) => {
                            test_identifier(expression, "x".to_owned())
                        }
                        other => panic!("expected ExpressionStatement but got {:?}", other),
                    };

                    match alternative {
                        Some(block) => {
                            assert_eq!(block.statements.len(), 1);
                            match &block.statements[0] {
                                Statement::Expr(ExpressionStatement { expression }) => {
                                    test_identifier(&expression, "y".to_owned())
                                }
                                other => panic!("expected ExpressionStatement but got {:?}", other),
                            }
                        }
                        None => panic!("expected Some but got None"),
                    }
                }
                other => panic!("expected IfExpression but got {:?}", other),
            },
            other => panic!("expected ExpressionStatement but got {:?}", other),
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y }";

        let lexer = Lexer::new(input);
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
                Expression::FunctionLiteral { parameters, body } => {
                    assert_eq!(parameters.len(), 2);
                    assert_eq!(parameters[0].0, "x".to_owned());
                    assert_eq!(parameters[1].0, "y".to_owned());
                    assert_eq!(body.statements.len(), 1);
                    match &body.statements[0] {
                        Statement::Expr(ExpressionStatement { expression }) => match expression {
                            InfixExpression {
                                left,
                                operator,
                                right,
                            } => {
                                assert_eq!(**left, Expression::Ident(Identifier("x".to_owned())));
                                assert_eq!(**right, Expression::Ident(Identifier("y".to_owned())));
                                assert_eq!(*operator, InfixOperator::PLUS)
                            }
                            other => panic!("expected InfixExpression but got {:?}", other),
                        },
                        other => panic!("expected ExpressionStatement but got {:?}", other),
                    }
                }
                other => panic!("expected FunctionLiteral but got {:?}", other),
            },
            other => panic!("expected ExpressionStatement but got {:?}", other),
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct TestInput(String, Vec<String>);

        let tests = vec![
            TestInput("fn() {};".to_owned(), vec![]),
            TestInput("fn(x) {};".to_owned(), vec!["x".to_owned()]),
            TestInput(
                "fn(x, y, z) {};".to_owned(),
                vec!["x".to_owned(), "y".to_owned(), "z".to_owned()],
            ),
        ];

        for TestInput(input, expected_parameters) in tests {
            let lexer = Lexer::new(&input);
            let program = Parser::new(lexer)
                .parse_program()
                .expect(&format!("program should be parsable, {}", input));

            let statements = program.statements;

            match &statements[0] {
                Statement::Expr(ExpressionStatement { expression }) => match expression {
                    FunctionLiteral {
                        parameters,
                        body: _,
                    } => {
                        assert_eq!(expected_parameters.len(), parameters.len());

                        for (i, param) in expected_parameters.iter().enumerate() {
                            assert_eq!(*param, expected_parameters[i])
                        }
                    }
                    other => panic!("expected FunctionLiteral but got {:?}", other),
                },
                other => panic!("expected ExpressionStatement but got {:?}", other),
            }
        }
    }

    #[test]
    fn test_parsing_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5)";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let statements = parser
            .parse_program()
            .expect(&format!("program should be parsable, {}", input))
            .statements;

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expr(ExpressionStatement { expression }) => match expression {
                CallExpression {
                    function,
                    arguments,
                } => {
                    assert_eq!(**function, Ident(Identifier("add".to_owned())));
                    assert_eq!(arguments.len(), 3);
                    test_literal_expression(&arguments[0], Int(1));
                    test_infix_expression(&arguments[1], Int(2), InfixOperator::ASTERISK, Int(3));
                    test_infix_expression(&arguments[2], Int(4), InfixOperator::PLUS, Int(5));
                }
                other => panic!("expected CallExpression but got {:?}", other),
            },
            other => panic!("expected ExpressionStatement but got {:?}", other),
        }
    }

    #[test]
    fn test_string_literal_expressions() {
        let input = r#" "hello world" "#;

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let statements = parser
            .parse_program()
            .expect(&format!("program should be parsable, {}", input))
            .statements;

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expr(ExpressionStatement { expression }) => match expression {
                StringLiteral { value } => assert_eq!(value, "hello world"),
                other => panic!("expected StringLiteral but got {:?}", other),
            },
            other => panic!("expected ExpressionStatement but got {:?}", other),
        }
    }

    #[test]
    fn test_array_literal_expressions() {
        let input = "[1, 2 * 2, 3 + 3];";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let statements = parser
            .parse_program()
            .expect(&format!("program should be parsable, {}", input))
            .statements;

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expr(ExpressionStatement { expression }) => match expression {
                ArrayLiteral { elements } => {
                    assert_eq!(elements.len(), 3);
                    test_integer_literal(&elements[0], 1);
                    test_infix_expression(&elements[1], Int(2), InfixOperator::ASTERISK, Int(2));
                    test_infix_expression(&elements[2], Int(3), InfixOperator::PLUS, Int(3));
                }
                other => panic!("expected ArrayLiteral but got {:?}", other),
            },
            other => panic!("expected ExpressionStatement but got {:?}", other),
        }
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1+1]";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let statements = parser
            .parse_program()
            .expect(&format!("program should be parsable, {}", input))
            .statements;

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expr(ExpressionStatement { expression }) => match expression {
                IndexExpression { left, index } => {
                    test_identifier(left, "myArray".to_owned());
                    test_infix_expression(index, Int(1), InfixOperator::PLUS, Int(1));
                }
                other => panic!("expected IndexExpression but got {:?}", other),
            },
            other => panic!("expected ExpressionStatement but got {:?}", other),
        }
    }
}
