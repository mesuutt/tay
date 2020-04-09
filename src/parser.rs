use std::fmt;
use crate::token::Token;
use crate::lexer::Lexer;
use crate::ast::{
    Program, Statement, Ident, Precedence, Expression,
    Literal, Prefix, Infix, FloatSize, IntegerSize,
};

#[derive(PartialEq, Debug)]
pub enum ParseError {
    InvalidToken(Token),
    InvalidSyntax(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::InvalidSyntax(error) => write!(f, "Invalid syntax: `{}`", error),
            ParseError::InvalidToken(token) => write!(f, "Invalid token: `{}`", token),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Parser {
            lexer,
            current_token: Token::EndOfFile,
            peek_token: Token::EndOfFile,
        };

        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse(&mut self) -> Program {
        let mut statements = vec![];
        let mut errors = vec![];
        while !self.current_token_is(Token::EndOfFile) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => errors.push(e)
            }
            self.next_token();
        }

        Program { statements, errors }
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == token
    }

    fn expect_peek(&mut self, token: Token) -> Result<bool, ParseError> {
        if self.peek_token == token {
            self.next_token();
            Ok(true)
        } else {
            Err(ParseError::InvalidToken(token))
        }
    }

    fn token_to_precedence(&self, token: &Token) -> Precedence {
        match token {
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Asterisk | Token::Slash | Token::Percent => Precedence::Product,
            Token::Caret => Precedence::Exponent,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest
        }
    }

    fn current_precedence(&self) -> Precedence {
        self.token_to_precedence(&self.current_token)
    }

    fn peek_precedence(&self) -> Precedence {
        self.token_to_precedence(&self.peek_token)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Ident(_) if self.peek_token_is(Token::Assign) => self.parse_let_statement(),

            _ => self.parse_expression_statement()
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        if let Token::Ident(_) = self.peek_token {
            // allowing variable assignment without let keyword
            self.next_token();
        }

        let ident = match self.parse_ident() {
            Ok(ident) => ident,
            Err(e) => return Err(e)
        };

        if let Err(e) = self.expect_peek(Token::Assign) {
            return Err(e);
        }

        self.next_token();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let(ident, expr))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return(expr))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        match self.parse_expression(Precedence::Lowest) {
            Ok(expr) => {
                if self.peek_token_is(Token::Semicolon) {
                    self.next_token();
                }
                Ok(Statement::Expression(expr))
            }
            Err(e) => Err(e),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        // we don't have infix/prefix functions, instead calling related fn with pattern matching
        let mut left = match self.current_token {
            Token::Ident(_) => self.parse_ident_expr(),
            Token::Int(_) => self.parse_integer_literal(),
            Token::Float(_) => self.parse_float_literal(),
            Token::Bool(_) => self.parse_boolean(),
            Token::Minus | Token::Bang => self.parse_prefix_expr(),
            Token::LParen => self.parse_grouped_expr(),
            Token::Illegal(_) => return Err(ParseError::InvalidToken(self.current_token.clone())),
            _ => {
                return Err(ParseError::InvalidToken(self.current_token.clone()));
            }
        };

        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
            match self.peek_token {
                Token::Slash
                | Token::Plus
                | Token::Asterisk
                | Token::Minus
                | Token::Percent
                | Token::Caret
                => {
                    self.next_token();
                    match left {
                        Ok(l) => left = self.parse_infix_expr(l),
                        Err(err) => return Err(err),
                    }
                }
                Token::LParen => {
                    self.next_token();
                    match left {
                        Ok(l) => left = self.parse_call_expr(l),
                        Err(err) => return Err(err),
                    }
                }
                _ => return left
            }
        }
        left
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        match self.current_token {
            Token::Ident(ref mut ident_str) => Ok(Ident(ident_str.clone())),
            _ => Err(ParseError::InvalidToken(self.current_token.clone()))
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParseError> {
        match &self.current_token {
            Token::Int(literal) => {
                match literal.parse::<IntegerSize>() {
                    Ok(x) => Ok(Expression::Literal(Literal::Int(x))),
                    Err(_) => Err(ParseError::InvalidSyntax(literal.clone()))
                }
            }
            _ => Err(ParseError::InvalidToken(self.current_token.clone()))
        }
    }

    fn parse_float_literal(&mut self) -> Result<Expression, ParseError> {
        match &self.current_token {
            Token::Float(literal) => {
                match &literal.parse::<FloatSize>() {
                    Ok(x) => Ok(Expression::Literal(Literal::Float(*x))),
                    Err(_) => Err(ParseError::InvalidSyntax(literal.clone()))
                }
            }
            _ => Err(ParseError::InvalidToken(self.current_token.clone()))
        }
    }

    fn parse_boolean(&self) -> Result<Expression, ParseError> {
        match self.current_token {
            Token::Bool(x) => Ok(Expression::Literal(Literal::Bool(x))),
            _ => Err(ParseError::InvalidToken(self.current_token.clone()))
        }
    }

    fn parse_ident_expr(&mut self) -> Result<Expression, ParseError> {
        match self.parse_ident() {
            Ok(ident) => Ok(Expression::Ident(ident)),
            Err(err) => Err(err),
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Expression, ParseError> {
        let prefix = match self.current_token {
            Token::Bang => Prefix::Bang,
            Token::Minus => Prefix::Minus,
            _ => return Err(ParseError::InvalidToken(self.current_token.clone()))
        };

        self.next_token();

        match self.parse_expression(Precedence::Prefix) {
            Ok(expr) => Ok(Expression::Prefix(prefix, Box::new(expr))),
            Err(e) => Err(e)
        }
    }

    fn parse_infix_expr(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let infix = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Asterisk => Infix::Mul,
            Token::Slash => Infix::Div,
            Token::Caret => Infix::Exponent,
            Token::Percent => Infix::Percent,
            _ => return Err(ParseError::InvalidToken(self.current_token.clone())),
        };

        let precedence = self.current_precedence();
        self.next_token();

        match self.parse_expression(precedence) {
            Ok(expr) => Ok(Expression::Infix(infix, Box::new(left), Box::new(expr))),
            Err(e) => Err(e)
        }
    }

    fn parse_call_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut args: Vec<Expression> = vec![];
        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();

        match self.parse_expression(Precedence::Lowest) {
            Ok(expr) => args.push(expr),
            Err(e) => return Err(e)
        }

        while self.expect_peek(Token::Comma).is_ok() {  // Used expect_peek instead peek_token_is
            self.next_token();
            match self.parse_expression(Precedence::Lowest) {
                Ok(expr) => args.push(expr),
                Err(e) => return Err(e)
            }
        }
        if !self.peek_token_is(Token::RParen) {
            Err(ParseError::InvalidToken(self.peek_token.clone()))
        } else {
            Ok(args)
        }
    }

    fn parse_call_expr(&mut self, func: Expression) -> Result<Expression, ParseError> {
        let args = match self.parse_call_args() {
            Ok(args) => args,
            Err(e) => return Err(e)
        };

        Ok(Expression::Call {
            func: Box::new(func),
            args,
        })
    }

    fn parse_grouped_expr(&mut self) -> Result<Expression, ParseError> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);
        if let Err(err) = self.expect_peek(Token::RParen) {
            Err(err)
        } else {
            expr
        }
    }
}


#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use super::Parser;
    use crate::ast::{Statement, Ident, Literal, Expression, Prefix, Infix};

    #[test]
    fn let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;
        let z = 1.2;
        "#;

        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 3);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Let(Ident(String::from("x")), Expression::Literal(Literal::Int(5))),
            Statement::Let(Ident(String::from("y")), Expression::Literal(Literal::Int(10))),
            Statement::Let(Ident(String::from("z")), Expression::Literal(Literal::Float(1.2))),
        ]);
    }

    #[test]
    fn return_statement() {
        let input = r#"
        return 1;
        return foo;
        "#;

        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 2);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Return(Expression::Literal(Literal::Int(1))),
            Statement::Return(Expression::Ident(Ident("foo".to_string()))),
        ]);
    }

    #[test]
    fn parser_errors() {
        let invalid_input = "let x 5;";
        let program = Parser::new(Lexer::new(invalid_input)).parse();
        assert_eq!(program.errors.len(), 1);
    }

    #[test]
    fn integer_float_literal_expr() {
        let program = Parser::new(Lexer::new("5;6.1;")).parse();
        let expected = vec![
            Statement::Expression(Expression::Literal(Literal::Int(5))),
            Statement::Expression(Expression::Literal(Literal::Float(6.1))),
        ];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn prefix_expr() {
        let program = Parser::new(Lexer::new("!5;-15;")).parse();
        let expected = vec![
            Statement::Expression(Expression::Prefix(Prefix::Bang, Box::new(Expression::Literal(Literal::Int(5))))),
            Statement::Expression(Expression::Prefix(Prefix::Minus, Box::new(Expression::Literal(Literal::Int(15))))),
        ];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn infix_expr() {
        let program = Parser::new(Lexer::new("5 + 15 * 18;")).parse();
        assert_eq!(0, program.errors.len());

        let expected = vec![
            Statement::Expression(
                Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Infix(
                        Infix::Mul,
                        Box::new(Expression::Literal(Literal::Int(15))),
                        Box::new(Expression::Literal(Literal::Int(18))),
                    )),
                ),
            ),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn operator_precedence() {
        let data = vec![
            ("-a * b", "((-a) * b)"),
            ("a ^ b % c * d", "(((a ^ b) % c) * d)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ];

        for &(input, expected) in data.iter() {
            let program = Parser::new(Lexer::new(input)).parse();
            assert_eq!(program.errors.len(), 0);
            assert_eq!(format!("{}", program.statements.first().unwrap()), expected);
        }
    }
}