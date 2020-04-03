mod env;
mod object;

use std::fmt;
pub use env::Env;
use object::Object;
use crate::ast;
use crate::token::{FloatSize, IntegerSize};
use crate::ast::Infix;

pub struct Evaluator {
    env: Env,
}

impl Evaluator {
    pub fn new(env: Env) -> Evaluator {
        Evaluator {
            env
        }
    }
    pub fn eval(&mut self, prog: ast::Program) -> Option<Object> {
        let mut result = None;
        for s in prog {
            match self.eval_statement(s) {
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                obj => {
                    result = obj
                }
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: ast::Statement) -> Option<Object> {
        match statement {
            ast::Statement::Expression(expr) => self.eval_expr(expr),
            ast::Statement::Let(ident, expr) => self.eval_let_statement(ident, expr),
            _ => None
        }
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
                let right = self.eval_expr(*right_expr);
                if left.is_some() && right.is_some() {
                    if let Some(Object::Error(_)) = left {
                        left
                    } else if let Some(Object::Error(_)) = right {
                        right
                    } else {
                        self.eval_infix_expr(operator, left.unwrap(), right.unwrap())
                    }
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
            ast::Literal::Float(x) => Object::Float(x),
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
                    Ok(obj) => Some(obj),
                    Err(err) => Some(Object::Error(err)),
                }
            }
            (Object::Float(x), Object::Float(y)) => {
                match self.eval_float_with_float_infix_expr(operator, x, y) {
                    Ok(obj) => Some(obj),
                    Err(err) => Some(Object::Error(err)),
                }
            }
            (Object::Float(x), Object::Int(y)) => {
                match self.eval_float_with_int_infix_expr(operator, x, y) {
                    Ok(obj) => Some(obj),
                    Err(err) => Some(Object::Error(err)),
                }
            }
            (Object::Int(x), Object::Float(y)) => {
                match self.eval_int_with_float_infix_expr(operator, x, y) {
                    Ok(obj) => Some(obj),
                    Err(err) => Some(Object::Error(err)),
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

    fn eval_integer_infix_expr(&self, operator: ast::Infix, left_val: i64, right_val: i64) -> Result<Object, String> {
        match operator {
            ast::Infix::Minus => Ok(Object::Int(left_val - right_val)),
            ast::Infix::Plus => Ok(Object::Int(left_val + right_val)),
            ast::Infix::Mul => Ok(Object::Int(left_val * right_val)),
            ast::Infix::Div => Ok(Object::Int(left_val / right_val)),
            ast::Infix::Percent => Ok(Object::Int(left_val % right_val)),
            ast::Infix::Exponent => {
                let (num, is_overflow) = (left_val as i32).overflowing_pow(right_val as u32);
                if is_overflow {
                    return Err(String::from("exponent too large"));
                } else {
                    Ok(Object::Int(num as i64))
                }
            }
        }
    }

    fn eval_float_with_float_infix_expr(&self, operator: ast::Infix, left_val: FloatSize, right_val: FloatSize) -> Result<Object, String> {
        match operator {
            ast::Infix::Minus => Ok(Object::Float(left_val - right_val)),
            ast::Infix::Plus => Ok(Object::Float(left_val + right_val)),
            ast::Infix::Mul => Ok(Object::Float(left_val * right_val)),
            ast::Infix::Div => Ok(Object::Float(left_val / right_val)),
            ast::Infix::Percent => Ok(Object::Float(left_val % right_val)),
            ast::Infix::Exponent => {
                let num = left_val.powf(right_val);
                Ok(Object::Float(num))
            }
        }
    }

    fn eval_float_with_int_infix_expr(&self, operator: ast::Infix, left_val: FloatSize, right_val: IntegerSize) -> Result<Object, String> {
        match operator {
            ast::Infix::Minus => Ok(Object::Float(left_val - right_val as FloatSize)),
            ast::Infix::Plus => Ok(Object::Float(left_val + right_val as FloatSize)),
            ast::Infix::Mul => Ok(Object::Float(left_val * right_val as FloatSize)),
            ast::Infix::Div => Ok(Object::Float(left_val / right_val as FloatSize)),
            ast::Infix::Percent => Ok(Object::Float(left_val % right_val as FloatSize)),
            ast::Infix::Exponent => {
                let num = left_val.powi(right_val as i32);
                Ok(Object::Float(num))
            }
        }
    }

    fn eval_int_with_float_infix_expr(&self, operator: ast::Infix, left_val: IntegerSize, right_val: FloatSize) -> Result<Object, String> {
        match operator {
            ast::Infix::Minus => Ok(Object::Float(left_val as FloatSize - right_val)),
            ast::Infix::Plus => Ok(Object::Float(left_val as FloatSize + right_val)),
            ast::Infix::Mul => Ok(Object::Float(left_val as FloatSize * right_val)),
            ast::Infix::Div => Ok(Object::Float(left_val as FloatSize / right_val)),
            ast::Infix::Percent => Ok(Object::Float(left_val as FloatSize % right_val)),
            ast::Infix::Exponent => {
                let num = left_val.pow(right_val as u32);
                Ok(Object::Float(num as FloatSize))
            }
        }
    }

    fn eval_let_statement(&mut self, ident: ast::Ident, expr: ast::Expression) -> Option<Object> {
        let value = match self.eval_expr(expr) {
            Some(val) => val,
            None => return None
        };

        if Self::is_error(&value) {
            return Some(value);
        }

        match ident {
            ast::Ident(name) => {
                self.env.set(name, &value);
                None
            }
            _ => None
        }
    }

    fn is_error(obj: &Object) -> bool {
        match obj {
            Object::Error(_) => true,
            _ => false
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
        let mut evaluator = Evaluator::new(env);
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

    #[test]
    fn float_expr() {
        let expected = vec![
            ("5.1", 5.1),
            ("5.2 * 5.1", 26.52),
            ("2 * 4.2", 8.4),
            ("4.2 * 2", 8.4),
            ("4.3 ^ 2", 18.49),
            ("2 ^ 4.3", 16.0),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Float(expected));
        }
    }

    #[test]
    fn let_statement() {
        let expected = vec![
            ("let x = 5; x;", 5),
            ("let x = 5 * 5; x;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Some(Object::Int(expected)));
        }
    }
}