mod env;
mod object;

pub use env::Env;
use object::Object;
use crate::ast;
use crate::ast::Expression;

pub struct Evaluator {
    env: Env,
}

impl Evaluator {
    pub fn new(env: Env) -> Evaluator {
        Evaluator {
            env
        }
    }
    pub fn eval(&self, prog: ast::Program) -> Option<Object> {
        for s in prog {
            match s {
                ast::Statement::Expression(expr) => return self.eval_expr(expr),
                _ => return None
            }
        }
        None
    }

    fn eval_expr(&self, expr: ast::Expression) -> Option<Object> {
        match expr {
            ast::Expression::Literal(lit) => Some(self.eval_literal(lit)),
            ast::Expression::Ident(ident) => Some(self.eval_ident(ident)),
            ast::Expression::Prefix(prefix, expr) => {
                if let Some(right) = self.eval_expr(*expr) {
                    self.eval_prefix_expr(prefix, right)
                } else {
                    None
                }
            }
            _ => None
        }
    }

    fn eval_literal(&self, literal: ast::Literal) -> Object {
        match literal {
            ast::Literal::Int(x) => Object::Int(x),
        }
    }

    fn eval_ident(&self, ident: ast::Ident) -> Object {
        let ast::Ident(name) = ident;
        match self.env.get(&name) {
            Some(val) => val,
            None => Object::Error(format!("identifier not found: {}", name))
        }
    }

    fn eval_prefix_expr(&self, operator: ast::Prefix, right: Object) -> Option<Object> {
        match operator {
            ast::Prefix::Minus => self.eval_minus_operator_expr(right),
            _ => return None
        }
    }

    fn eval_minus_operator_expr(&self, right: Object) -> Option<Object> {
        match right {
            Object::Int(x) => Some(Object::Int(-x)),
            _ => None
        }
    }
}


#[cfg(test)]
mod test {
    use super::Env;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use super::{Evaluator, Object};

    fn test_eval(input: &str) -> Option<Object> {
        let prog = Parser::new(Lexer::new(input)).parse();
        let env = Env::new();
        let evaluator = Evaluator::new(env);
        evaluator.eval(prog)
    }

    #[test]
    fn integer_expr() {
        let expected = vec![
            ("5", 5),
            ("10", 10),
            ("-10", -10),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Int(expected));
        }
    }
}