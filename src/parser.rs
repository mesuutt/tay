use crate::token::Token;
use crate::lexer::Lexer;
use crate::ast::{self, Program, Statement, Ident, Precedence, Expression, Literal};


struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Parser {
            lexer,
            current_token: Token::EndOfFile,
            peek_token: Token::EndOfFile,
            errors: vec![],
        };

        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == token
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token == token {
            self.next_token();
            return true;
        } else {
            self.peek_error(token);
            false
        }
    }

    fn peek_error(&mut self, token: Token) {
        let msg = format!("expected token: '{}', got '{}'", token, self.peek_token);
        self.errors.push(msg)
    }

    pub fn get_errors(&self) -> Vec<String> {
        return self.errors.clone();
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();
        while !self.current_token_is(Token::EndOfFile) {
            match self.parse_statement() {
                Some(stmt) => program.push(stmt),
                _ => {}
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        // If next token is not identifier return
        match self.peek_token {
            Token::Ident(_) => self.next_token(),
            _ => return None,
        };

        let ident = match self.parse_ident() {
            Some(ident) => ident,
            _ => return None
        };

        // let ident = Ident(String::from(self.current_token.to_string()));

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        self.next_token();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            _ => return None,
        };

        // let expr = Expression::Ident(Ident(String::from(self.current_token.to_string())));
        Some(Statement::Let(ident, expr))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        match self.parse_expression(Precedence::Lowest) {
            Some(expr) => {
                if self.peek_token_is(Token::Semicolon) {
                    self.next_token();
                }
                Some(Statement::Expression(expr))
            }
            _ => None
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        // we don't have infix/prefix functions, instead calling related fn with pattern matching
        match self.current_token {
            Token::Ident(_) => self.parse_ident_expr(),
            Token::Int(_) => self.parse_integer_literal(),
            _ => return None
        }
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        match self.current_token {
            Token::Ident(ref mut ident_str) => Some(Ident(ident_str.clone())),
            _ => None
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        match self.current_token {
            Token::Int(num) => Some(Expression::Literal(Literal::Int(num.clone()))),
            _ => None
        }
    }

    fn parse_ident_expr(&mut self) -> Option<Expression> {
        match self.parse_ident() {
            Some(ident) => Some(Expression::Ident(ident.clone())),
            _ => None
        }
    }
}


#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use super::Parser;
    use crate::ast::{Statement, Ident, Literal, Expression};

    #[test]
    fn let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;"#;

        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse();
        assert_eq!(prog.len(), 2);
        assert_eq!(p.get_errors().len(), 0);

        assert_eq!(prog, vec![
            Statement::Let(Ident(String::from("x")), Expression::Literal(Literal::Int(5))),
            Statement::Let(Ident(String::from("y")), Expression::Literal(Literal::Int(10))),
        ]);
    }

    #[test]
    fn parser_errors() {
        let invalid_input = "let x 5;";
        let mut p = Parser::new(Lexer::new(invalid_input));
        let prog = p.parse();
        assert_eq!(p.get_errors().len(), 1);
        assert_eq!(prog.len(), 0);
    }

    #[test]
    fn parser_integer_literal_expr() {
        let invalid_input = "5;6;";
        let mut p = Parser::new(Lexer::new(invalid_input));
        let prog = p.parse();
        let expected = vec![
            Statement::Expression(Expression::Literal(Literal::Int(5))),
            Statement::Expression(Expression::Literal(Literal::Int(6))),
        ];
        assert_eq!(prog, expected);
    }
}