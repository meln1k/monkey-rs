use crate::ast::ast::Expression::{CallExpression, FunctionLiteral, IfExpression, InfixExpression};
use crate::ast::ast::{
    BlockStatement, Expression, ExpressionStatement, LetStatement, Operator, Program,
    ReturnStatement, Statement,
};
use crate::lexer::lexer::Lexer;
use crate::lexer::token::Token;
use crate::parser::Precedence::{LOWEST, PREFIX};

type ParsingError = String;
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

    pub fn parse_program(mut self) -> Result<Program, ParsingErrors> {
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
        let name = self.expect_ident()?;

        self.expect_peek(Token::ASSIGN)?;

        self.advance_token();

        let value = self.parse_expression(LOWEST)?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.advance_token();
        }

        Ok(LetStatement { name, value })
    }

    fn parse_return_statement(&mut self) -> ParsingResult<ReturnStatement> {
        self.advance_token();

        let return_value = self.parse_expression(LOWEST)?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.advance_token();
        }

        Ok(ReturnStatement { return_value })
    }

    fn parse_expr_statement(&mut self) -> ParsingResult<ExpressionStatement> {
        let maybe_expr = self.parse_expression(LOWEST);

        if self.peek_token_is(&Token::SEMICOLON) {
            self.advance_token()
        }

        maybe_expr.map(|expression| ExpressionStatement { expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParsingResult<Expression> {
        let mut left_expr = match self.cur_token {
            Token::IDENT(_) => Parser::parse_identifier(self),
            Token::INT(_) => Parser::parse_integer_literal(self),
            Token::BANG | Token::MINUS => Parser::parse_prefix_expression(self),
            Token::TRUE | Token::FALSE => Parser::parse_boolean(self),
            Token::LPAREN => Parser::parse_grouped_expression(self),
            Token::IF => Parser::parse_if_expression(self),
            Token::FUNCTION => Parser::parse_function_literal(self),
            _ => Err(format!(
                "no prefix parse function for {:?} found",
                self.cur_token
            )),
        }?;

        while !self.current_token_is(Token::SEMICOLON) && precedence < precedences(&self.peek_token)
        {
            match &self.peek_token {
                Token::PLUS
                | Token::MINUS
                | Token::SLASH
                | Token::ASTERISK
                | Token::EQ
                | Token::NOT_EQ
                | Token::LT
                | Token::GT => {
                    self.advance_token();
                    left_expr = self.parse_infix_expression(left_expr)?;
                }
                Token::LPAREN => {
                    self.advance_token();
                    left_expr = self.parse_call_expression(left_expr)?;
                }
                _ => return Ok(left_expr),
            }
        }

        Ok(left_expr)
    }

    fn parse_identifier(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token {
            Token::IDENT(name) => Ok(Expression::Identifier(name.clone())),
            other => Err(format!("expected identifier but got {:?}", other)),
        }
    }

    fn parse_boolean(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token {
            Token::TRUE => Ok(Expression::Boolean(true)),
            Token::FALSE => Ok(Expression::Boolean(false)),
            other => Err(format!("expected boolean but got {:?}", other)),
        }
    }

    fn parse_integer_literal(&mut self) -> ParsingResult<Expression> {
        match &self.cur_token {
            Token::INT(value) => value
                .parse()
                .map_err(|_| format!("can't parse value {:?} as int", value))
                .map(Expression::IntegerLiteral),
            other => Err(format!("Expected INT but got {:?}", other)),
        }
    }

    fn parse_grouped_expression(&mut self) -> ParsingResult<Expression> {
        self.advance_token();

        let expr = self.parse_expression(LOWEST)?;

        self.expect_peek(Token::RPAREN)?;

        Ok(expr)
    }

    fn parse_prefix_expression(&mut self) -> ParsingResult<Expression> {
        let operator = match &self.cur_token {
            Token::BANG => Ok(Operator::BANG),
            Token::MINUS => Ok(Operator::MINUS),
            other => Err(format!("Expected prefix token but got {:?}", other)),
        }?;

        self.advance_token();

        self.parse_expression(PREFIX)
            .map(|expr| Expression::PrefixExpression {
                operator,
                expr: Box::new(expr),
            })
    }

    fn parse_if_expression(&mut self) -> ParsingResult<Expression> {
        self.expect_peek(Token::LPAREN)?;
        self.advance_token();
        let condition = Box::new(self.parse_expression(LOWEST)?);
        self.expect_peek(Token::RPAREN)?;
        self.expect_peek(Token::LBRACE)?;
        let consequence = Box::new(self.parse_block_statement()?);

        let alternative = if self.peek_token_is(&Token::ELSE) {
            self.advance_token();
            self.expect_peek(Token::LBRACE)?;
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

        while !self.current_token_is(Token::RBRACE) && !self.current_token_is(Token::EOF) {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.advance_token();
        }
        Ok(BlockStatement { statements })
    }

    fn parse_function_literal(&mut self) -> ParsingResult<Expression> {
        self.expect_peek(Token::LPAREN)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::LBRACE)?;

        let body = self.parse_block_statement()?;

        Ok(FunctionLiteral { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> ParsingResult<Vec<String>> {
        let mut identifiers: Vec<String> = Vec::new();

        if self.peek_token_is(&Token::RPAREN) {
            self.advance_token();
            return Ok(identifiers);
        }

        self.advance_token();

        let ident = match &self.cur_token {
            Token::IDENT(value) => Ok(value.clone()),
            other => Err(format!("expected identifier but got {:?}", other)),
        }?;

        identifiers.push(ident);

        while self.peek_token_is(&Token::COMMA) {
            self.advance_token();
            self.advance_token();

            let ident = match &self.cur_token {
                Token::IDENT(value) => Ok(value.clone()),
                other => Err(format!("expected identifier but got {:?}", other)),
            }?;

            identifiers.push(ident);
        }

        self.expect_peek(Token::RPAREN)?;

        Ok(identifiers)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> ParsingResult<Expression> {
        let operator = match &self.cur_token {
            Token::PLUS => Ok(Operator::PLUS),
            Token::MINUS => Ok(Operator::MINUS),
            Token::SLASH => Ok(Operator::SLASH),
            Token::ASTERISK => Ok(Operator::ASTERISK),
            Token::EQ => Ok(Operator::EQ),
            Token::NOT_EQ => Ok(Operator::NOT_EQ),
            Token::LT => Ok(Operator::LT),
            Token::GT => Ok(Operator::GT),
            other => Err(format!("Expected infix operator but got {:?}", other)),
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
        let arguments = self.parse_call_arguments()?;
        let function = Box::new(func);
        Ok(CallExpression {
            function,
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> ParsingResult<Vec<Expression>> {
        let mut arguments = Vec::new();
        if self.peek_token_is(&Token::RPAREN) {
            self.advance_token();
            return Ok(arguments);
        }

        self.advance_token();
        arguments.push(self.parse_expression(LOWEST)?);

        while self.peek_token_is(&Token::COMMA) {
            self.advance_token();
            self.advance_token();
            arguments.push(self.parse_expression(LOWEST)?);
        }

        self.expect_peek(Token::RPAREN)?;

        Ok(arguments)
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
            Ok(self.advance_token())
        } else {
            Err(format!(
                "Expected next token to be {:?} but got {:?}",
                t, self.peek_token
            ))
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
        Token::LPAREN => CALL,
        _ => LOWEST,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::ast::Expression::*;
    use crate::ast::ast::Operator::LT;
    use crate::ast::ast::{
        Expression, ExpressionStatement, LetStatement, Operator, ReturnStatement, Statement,
    };
    use crate::lexer::lexer::Lexer;
    use crate::parser::tests::ExpectedExprValue::{Bool, Int, Str};
    use crate::parser::Parser;

    #[test]
    fn test_let_statements() {
        struct Test(String, String, ExpectedExprValue);

        let tests = vec![
            Test("let x = 5;".to_owned(), "x".to_owned(), Int(5)),
            Test("let y = true;".to_owned(), "y".to_owned(), Bool(true)),
            Test(
                "let foobar = y;".to_owned(),
                "foobar".to_owned(),
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

            test_let_statement(statement, &expected_identifier);

            match statement {
                Statement::Let(LetStatement { name: _, value }) => {
                    test_literal_expression(value, expected_value);
                }
                other => panic!("Expected Let but got {:?}", other),
            }
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

        let tests = vec![5, 10, 42];

        for (id, expected) in tests.iter().enumerate() {
            match &statements[id] {
                Statement::Return(ReturnStatement { return_value }) => {
                    test_literal_expression(return_value, Int(*expected))
                },
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
        type Value = ExpectedExprValue;

        struct PrefixTest(Input, Operator, Value);

        use ExpectedExprValue::*;

        let prefix_tests = vec![
            PrefixTest("!5;".to_owned(), Operator::BANG, Int(5)),
            PrefixTest("-15;".to_owned(), Operator::MINUS, Int(15)),
            PrefixTest("!true;".to_owned(), Operator::BANG, Bool(true)),
            PrefixTest("!false;".to_owned(), Operator::BANG, Bool(false)),
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
            Expression::Identifier(s) => assert_eq!(*s, value),
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
        op: Operator,
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
        use Operator::*;

        struct InfixTest(Input, LeftValue, Operator, RightValue);

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
            Test("true".to_owned(), "true".to_owned()),
            Test("false".to_owned(), "false".to_owned()),
            Test("3 > 5 == false".to_owned(), "((3 > 5) == false)".to_owned()),
            Test("3 < 5 == true".to_owned(), "((3 < 5) == true)".to_owned()),
            Test(
                "1 + (2 + 3) + 4".to_owned(),
                "((1 + (2 + 3)) + 4)".to_owned(),
            ),
            Test("(5 + 5) * 2".to_owned(), "((5 + 5) * 2)".to_owned()),
            Test("2 / (5 + 5)".to_owned(), "(2 / (5 + 5))".to_owned()),
            Test("-(5 + 5)".to_owned(), "(-(5 + 5))".to_owned()),
            Test("!(true == true)".to_owned(), "(!(true == true))".to_owned()),
            Test(
                "a + add(b * c) + d".to_owned(),
                "((a + add((b * c))) + d)".to_owned(),
            ),
            Test(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".to_owned(),
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_owned(),
            ),
            Test(
                "add(a + b + c * d / f + g)".to_owned(),
                "add((((a + b) + ((c * d) / f)) + g))".to_owned(),
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
                    test_infix_expression(condition, Str("x".to_owned()), LT, Str("y".to_owned()));
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
                    test_infix_expression(condition, Str("x".to_owned()), LT, Str("y".to_owned()));
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
                    assert_eq!(parameters[0], "x".to_owned());
                    assert_eq!(parameters[1], "y".to_owned());
                    assert_eq!(body.statements.len(), 1);
                    match &body.statements[0] {
                        Statement::Expr(ExpressionStatement { expression }) => match expression {
                            InfixExpression {
                                left,
                                operator,
                                right,
                            } => {
                                assert_eq!(**left, Expression::Identifier("x".to_owned()));
                                assert_eq!(**right, Expression::Identifier("y".to_owned()));
                                assert_eq!(*operator, Operator::PLUS)
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
                    FunctionLiteral { parameters, body: _ } => {
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
                    assert_eq!(**function, Identifier("add".to_owned()));
                    assert_eq!(arguments.len(), 3);
                    test_literal_expression(&arguments[0], Int(1));
                    test_infix_expression(&arguments[1], Int(2), Operator::ASTERISK, Int(3));
                    test_infix_expression(&arguments[2], Int(4), Operator::PLUS, Int(5));
                }
                other => panic!("expected CallExpression but got {:?}", other),
            },
            other => panic!("expected ExpressionStatement but got {:?}", other),
        }
    }
}
