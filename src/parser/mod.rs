#[cfg(test)]
mod test;

use std::{fmt, mem};
use crate::lexer::{Token, Lexer};
use crate::ast::{Program, Statement, Ident, Precedence, Expression, Literal, Prefix, Infix, FloatSize, IntegerSize, BlockStatement};

#[derive(PartialEq, Debug)]
pub enum ParseError {
    InvalidToken(Token),
    InvalidSyntax(String),
    NoPrefixParseFn(Token),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::InvalidSyntax(error) => write!(f, "Invalid syntax: `{}`", error),
            ParseError::InvalidToken(token) => write!(f, "Invalid token: `{}`", token),
            ParseError::NoPrefixParseFn(token) => write!(f, "no prefix parse function for {} found", token),
        }
    }
}


type ParseResult<T> = Result<T, ParseError>;
type PrefixParseFn = fn(&mut Parser) -> ParseResult<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> ParseResult<Expression>;


pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
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
        self.current_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
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
        // TODO: should be get Token as ref
        self.peek_token == token
    }

    fn expect_peek(&mut self, token: Token) -> Result<bool, ParseError> {
        // TODO: should be get Token as ref
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
            Token::Eq | Token::NotEq => Precedence::Equal,
            Token::Lt | Token::Gt | Token::Lte | Token::Gte => Precedence::LessGreater,
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

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Ident(_) if self.peek_token_is(Token::Assign) => self.parse_let_statement(),

            _ => self.parse_expression_statement()
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
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

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
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

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
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

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let prefix = self.prefix_parse_fn().ok_or_else(|| ParseError::NoPrefixParseFn(self.current_token.clone()))?;
        let mut left_expr = prefix(self)?;

        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
            if let Some(infix) = self.infix_parse_fn() {
                self.next_token();
                left_expr = infix(self, left_expr)?;
            }
        }

        Ok(left_expr)
    }

    fn prefix_parse_fn(&self) -> Option<PrefixParseFn> {
        let left = match &self.current_token {
            Token::Ident(_) => Parser::parse_ident_expr,
            Token::String(_) => Parser::parse_string_literal,
            Token::Int(_) => Parser::parse_integer_literal,
            Token::Float(_) => Parser::parse_float_literal,
            Token::Bool(_) => Parser::parse_boolean,
            Token::Minus | Token::Bang => Parser::parse_prefix_expr,
            Token::LParen => Parser::parse_grouped_expr,
            Token::If => Parser::parse_if_expr,
            Token::Function => Parser::parse_func_literal,
            _ => return None
        };

        Some(left)
    }

    fn infix_parse_fn(&self) -> Option<InfixParseFn> {
        Some(match &self.peek_token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Percent
            | Token::Caret
            | Token::Eq
            | Token::NotEq
            | Token::Lt
            | Token::Gt
            | Token::Lte
            | Token::Gte => Parser::parse_infix_expr,
            Token::LParen => Parser::parse_call_expr,
            _ => return None
        })
    }

    fn parse_ident(&mut self) -> ParseResult<Ident> {
        match self.current_token {
            Token::Ident(ref mut ident_str) => Ok(Ident(ident_str.clone())),
            _ => Err(ParseError::InvalidToken(self.current_token.clone()))
        }
    }

    fn parse_string_literal(&mut self) -> ParseResult<Expression> {
        match &self.current_token {
            Token::String(literal) => Ok(Expression::Literal(Literal::String(literal.clone()))),
            _ => Err(ParseError::InvalidToken(self.current_token.clone())) // FIXME: unreachable
        }
    }

    fn parse_integer_literal(&mut self) -> ParseResult<Expression> {
        match &self.current_token {
            Token::Int(literal) => {
                match literal.parse::<IntegerSize>() {
                    Ok(x) => Ok(Expression::Literal(Literal::Int(x))),
                    Err(_) => Err(ParseError::InvalidSyntax(literal.clone()))
                }
            }
            _ => Err(ParseError::InvalidToken(self.current_token.clone())) // FIXME: unreachable
        }
    }

    fn parse_float_literal(&mut self) -> ParseResult<Expression> {
        match &self.current_token {
            Token::Float(literal) => {
                match &literal.parse::<FloatSize>() {
                    Ok(x) => Ok(Expression::Literal(Literal::Float(*x))),
                    Err(_) => Err(ParseError::InvalidSyntax(literal.clone()))
                }
            }
            _ => Err(ParseError::InvalidToken(self.current_token.clone())) // FIXME: unreachable
        }
    }

    fn parse_boolean(&mut self) -> ParseResult<Expression> {
        match self.current_token {
            Token::Bool(x) => Ok(Expression::Literal(Literal::Bool(x))),
            _ => Err(ParseError::InvalidToken(self.current_token.clone())) // FIXME: unreachable
        }
    }

    fn parse_ident_expr(&mut self) -> ParseResult<Expression> {
        match self.parse_ident() {
            Ok(ident) => Ok(Expression::Ident(ident)),
            Err(err) => Err(err),
        }
    }

    fn parse_prefix_expr(&mut self) -> ParseResult<Expression> {
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

    fn parse_infix_expr(&mut self, left: Expression) -> ParseResult<Expression> {
        let infix = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Asterisk => Infix::Mul,
            Token::Slash => Infix::Div,
            Token::Caret => Infix::Exponent,
            Token::Percent => Infix::Percent,
            Token::Lt => Infix::Lt,
            Token::Gt => Infix::Gt,
            Token::Lte => Infix::Lte,
            Token::Gte => Infix::Gte,
            Token::Eq => Infix::Eq,
            Token::NotEq => Infix::NotEq,
            _ => return Err(ParseError::InvalidToken(self.current_token.clone())),
        };

        let precedence = self.current_precedence();
        self.next_token();

        match self.parse_expression(precedence) {
            Ok(expr) => Ok(Expression::Infix(infix, Box::new(left), Box::new(expr))),
            Err(e) => Err(e)
        }
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>, ParseError> {
        let mut args: Vec<Expression> = vec![];
        if self.peek_token_is(end.clone()) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();

        match self.parse_expression(Precedence::Lowest) {
            Ok(expr) => args.push(expr),
            Err(e) => return Err(e)
        }

        while self.expect_peek(Token::Comma).is_ok() {  // Used expect_peek instead peek_token_is
            self.next_token(); // skip comma
            match self.parse_expression(Precedence::Lowest) {
                Ok(expr) => args.push(expr),
                Err(e) => return Err(e)
            }
        }

        if self.expect_peek(end).is_err() {
            Err(ParseError::InvalidToken(self.peek_token.clone()))
        } else {
            Ok(args)
        }
    }

    fn parse_call_expr(&mut self, func: Expression) -> ParseResult<Expression> {
        let args = match self.parse_expression_list(Token::RParen) {
            Ok(args) => args,
            Err(e) => return Err(e)
        };

        Ok(Expression::Call {
            func: Box::new(func),
            args,
        })
    }

    fn parse_func_literal(&mut self) -> ParseResult<Expression> {
        let mut identifier = None;
        if self.expect_peek(Token::LParen).is_err() {
            if let Token::Ident(_ident) = self.peek_token.clone() {
                self.next_token(); // skip fn
                identifier = match self.parse_ident() {
                    Ok(i) => {
                         self.next_token(); // skip ident
                        Some(i)
                    },
                    Err(e) => return Err(e)
                };
            } else {
                return Err(ParseError::InvalidToken(self.peek_token.clone()));
            }
        }

        let params = match self.parse_func_params() {
            Ok(prs) => prs,
            Err(e) => return Err(e),
        };

        if self.expect_peek(Token::LBrace).is_err() {
            return Err(ParseError::InvalidToken(self.peek_token.clone()));
        }

        let body = match self.parse_block_statement() {
            Ok(stmt) => stmt,
            Err(e) => return Err(e),
        };

        Ok(Expression::Func {
            identifier,
            params,
            body,
        })
    }

    fn parse_func_params(&mut self) -> ParseResult<Vec<Ident>> {
        let mut idents = vec![];

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Ok(idents);
        }

        self.next_token();

        match self.parse_ident() {
            Ok(id) => idents.push(id),
            Err(e) => return Err(e),
        }

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            match self.parse_ident() {
                Ok(id) => idents.push(id),
                Err(e) => return Err(e),
            }
        }

        if self.expect_peek(Token::RParen).is_err() {
            return Err(ParseError::InvalidToken(self.peek_token.clone()));
        }

        Ok(idents)
    }

    fn parse_grouped_expr(&mut self) -> ParseResult<Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);
        if let Err(err) = self.expect_peek(Token::RParen) {
            Err(err)
        } else {
            expr
        }
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expression> {
        if self.expect_peek(Token::LParen).is_err() {
            return Err(ParseError::InvalidSyntax(self.peek_token.to_string()));
        }
        self.next_token();

        let condition = match self.parse_expression(Precedence::Lowest) {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };

        if self.expect_peek(Token::RParen).is_err() || self.expect_peek(Token::LBrace).is_err() {
            return Err(ParseError::InvalidSyntax(self.peek_token.to_string()));
        }

        let consequence = match self.parse_block_statement() {
            Ok(stmt) => stmt,
            Err(err) => return Err(err),
        };

        let mut alternative = None;

        if self.peek_token_is(Token::Else) {
            self.next_token();
            if self.expect_peek(Token::LBrace).is_err() {
                return Err(ParseError::InvalidToken(self.peek_token.clone()));
            }

            match self.parse_block_statement() {
                Ok(stmt) => alternative = Some(stmt),
                Err(err) => return Err(err),
            }
        }

        Ok(Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }


    fn parse_block_statement(&mut self) -> ParseResult<BlockStatement> {
        self.next_token();
        let mut statements: BlockStatement = vec![];

        while !self.current_token_is(Token::RBrace) && !self.current_token_is(Token::EndOfFile) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => return Err(err),
            }
            self.next_token();
        }

        Ok(statements)
    }
}