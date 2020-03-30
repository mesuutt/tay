mod env;
mod object;

use std::fmt;
pub use env::Env;
use object::Object;
use crate::ast;

#[derive(Debug)]
struct EvalError {
    message: String
}

impl EvalError {
    fn new(msg: &str) -> EvalError {
        EvalError{message: msg.to_string()}
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,"{}",self.message)
    }
}

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
            ast::Expression::Prefix(operator, expr) => {
                if let Some(right) = self.eval_expr(*expr) {
                    self.eval_prefix_expr(operator, right)
                } else {
                    None
                }
            }
            ast::Expression::Infix(operator, left_expr, right_expr) => {
                let left = self.eval_expr(*left_expr);
                let rigth = self.eval_expr(*right_expr);
                if left.is_some() && rigth.is_some() {
                    self.eval_infix_expr(operator, left.unwrap(), rigth.unwrap())
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
            ast::Prefix::Minus => self.eval_minus_prefix_operator_expr(right),
            _ => return None
        }
    }

    fn eval_infix_expr(&self, operator: ast::Infix, left: Object, right: Object) -> Option<Object> {
        match (left, right) {
            (Object::Int(x), Object::Int(y)) => {
                match self.eval_integer_infix_expr(operator, x, y) {
                    Ok(obj ) => Some(obj),
                    Err(err) => Some(Object::Error(err.message)),
                }
            }
            _ => None
        }
    }

    fn eval_minus_prefix_operator_expr(&self, right: Object) -> Option<Object> {
        match right {
            Object::Int(x) => Some(Object::Int(-x)),
            _ => None
        }
    }

    fn eval_integer_infix_expr(&self, operator: ast::Infix, left_val: i64, right_val: i64) -> Result<Object, EvalError> {
        let aa = match operator {
            ast::Infix::Minus => Ok(Object::Int(left_val - right_val)),
            ast::Infix::Plus => Ok(Object::Int(left_val + right_val)),
            ast::Infix::Mul => Ok(Object::Int(left_val * right_val)),
            ast::Infix::Div => Ok(Object::Int(left_val / right_val)),
            ast::Infix::Percent => Ok(Object::Int(left_val % right_val)),
            ast::Infix::Exponent => {
                let (num, is_overflow) = (left_val as i32).overflowing_pow(right_val as u32);
                if is_overflow {
                    return Err(EvalError::new("exponent too large"))
                } else {
                    Ok(Object::Int(num as i64))
                }
            }
        };

        aa
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
            ("5 + 5 - 5", 5),
            ("(5 + 5) * 5", 50),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Int(expected));
        }
    }
}