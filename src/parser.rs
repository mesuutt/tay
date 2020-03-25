use crate::token::Token;
use crate::lexer::Lexer;
use crate::ast::{self, Program, Statement, Ident, Precedence, Expression};


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
        return self.errors.clone()
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
            _ => None
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        // If next token is not identifier return
        match self.peek_token {
            Token::Ident(_) => self.next_token(),
            _ => return None,
        };

        /*let ident = match self.parse_ident() {
            Some(name) => name,
            _ => return None
        };*/

        let ident = Ident(String::from(self.current_token.to_string()));

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        self.next_token();

        /*let expr = match self.parse_expression(ast::Precedence::Lowest) {
            Some(expr) => expr,
            _ => return None,
        };*/

        let expr = Expression::Ident(Ident(String::from(self.current_token.to_string())));
        Some(Statement::Let(ident, expr))
    }

    fn parse_expression(precedence: Precedence) {}
}


#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use super::Parser;
    use crate::ast::{Statement, Expression, Ident};

    #[test]
    fn let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;"#;

        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse();
        let expected = vec!["Identifier(x)", "Identifier(y)"];
        assert_eq!(prog.len(), 2);

        for (i, s) in prog.iter().enumerate() {
            if let Statement::Let(Ident(var_name), _) = s {
                assert_eq!(*var_name, expected[i]);
            }
        }
    }
}