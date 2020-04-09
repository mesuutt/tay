mod env;
mod object;
mod error;

pub use env::Env;
use object::Object;
use crate::ast;
use crate::evaluator::error::EvalError;

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
        for s in prog.statements {
            match self.eval_statement(s) {
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                Some(Object::Return(val)) => return Some(*val),
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
            ast::Statement::Return(expr) => self.eval_return_statement(expr),
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
                match (&left, &right) {
                    (Some(Object::Error(_)), _) => left,
                    (_, Some(Object::Error(_))) => right,
                    _ => self.eval_infix_expr(operator, left.unwrap(), right.unwrap())
                }
            }
            _ => None
        }
    }

    fn eval_literal(&self, literal: ast::Literal) -> Object {
        match literal {
            ast::Literal::Int(x) => Object::Int(x),
            ast::Literal::Float(x) => Object::Float(x),
            ast::Literal::Bool(x) => Object::Bool(x),
        }
    }

    fn eval_ident(&self, ident: ast::Ident) -> Object {
        let ast::Ident(name) = ident;
        match self.env.get(&name) {
            Some(val) => val,
            None => Object::Error(EvalError::UndefinedIdent(name.clone()))
        }
    }

    fn eval_prefix_expr(&self, operator: ast::Prefix, right: Object) -> Option<Object> {
        match operator {
            ast::Prefix::Minus => self.eval_minus_prefix_operator_expr(right),
            ast::Prefix::Bang => self.eval_bang_operator_expr(right),
            _ => None
        }
    }

    fn eval_infix_expr(&self, operator: ast::Infix, left: Object, right: Object) -> Option<Object> {
        match (left, right) {
            (Object::Int(x), Object::Int(y)) => {
                Some(self.eval_integer_infix_expr(operator, x, y))
            }
            (Object::Float(x), Object::Float(y)) => {
                Some(self.eval_float_infix_expr(operator, x, y))
            }
            (Object::Float(x), Object::Int(y)) => {
                Some(self.eval_float_with_int_infix_expr(operator, x, y))
            }
            (Object::Int(x), Object::Float(y)) => {
                Some(self.eval_int_with_float_infix_expr(operator, x, y))
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

    fn eval_bang_operator_expr(&self, right: Object) -> Option<Object> {
        match right {
            Object::Bool(true) => Some(Object::Bool(false)),
            Object::Bool(false) => Some(Object::Bool(true)),
            _ => None
        }
    }


    fn eval_integer_infix_expr(&self, operator: ast::Infix, left_val: i64, right_val: i64) -> Object {
        match operator {
            ast::Infix::Minus => Object::Int(left_val - right_val),
            ast::Infix::Plus => Object::Int(left_val + right_val),
            ast::Infix::Mul => Object::Int(left_val * right_val),
            ast::Infix::Div => {
                if right_val == 0 {
                    return Object::Error(EvalError::DivideByZero);
                }
                Object::Int(left_val / right_val)
            }
            ast::Infix::Percent => Object::Int(left_val % right_val),
            ast::Infix::Exponent => {
                let (num, is_overflow) = (left_val as i32).overflowing_pow(right_val as u32);
                if is_overflow {
                    Object::Error(EvalError::ExponentTooLarge)
                } else {
                    Object::Int(num as i64)
                }
            }
        }
    }

    fn eval_float_infix_expr(&self, operator: ast::Infix, left_val: ast::FloatSize, right_val: ast::FloatSize) -> Object {
        match operator {
            ast::Infix::Minus => Object::Float(left_val - right_val),
            ast::Infix::Plus => Object::Float(left_val + right_val),
            ast::Infix::Mul => Object::Float(left_val * right_val),
            ast::Infix::Div => {
                if right_val == 0.0 {
                    return Object::Error(EvalError::DivideByZero);
                }
                Object::Float(left_val / right_val)
            }
            ast::Infix::Percent => Object::Float(left_val % right_val),
            ast::Infix::Exponent => {
                let num = left_val.powf(right_val);
                Object::Float(num)
            }
        }
    }

    fn eval_float_with_int_infix_expr(&self, operator: ast::Infix, left_val: ast::FloatSize, right_val: ast::IntegerSize) -> Object {
        match operator {
            ast::Infix::Minus => Object::Float(left_val - right_val as ast::FloatSize),
            ast::Infix::Plus => Object::Float(left_val + right_val as ast::FloatSize),
            ast::Infix::Mul => Object::Float(left_val * right_val as ast::FloatSize),
            ast::Infix::Div => Object::Float(left_val / right_val as ast::FloatSize),
            ast::Infix::Percent => Object::Float(left_val % right_val as ast::FloatSize),
            ast::Infix::Exponent => {
                // let num = left_val.powi(right_val as i32);
                // Ok(Object::Float(num))
                Object::Error(EvalError::TypeError("unsupported operand types for ^: 'float' and 'int'".to_string()))
            }
        }
    }

    fn eval_int_with_float_infix_expr(&self, operator: ast::Infix, left_val: ast::IntegerSize, right_val: ast::FloatSize) -> Object {
        match operator {
            ast::Infix::Minus => Object::Float(left_val as ast::FloatSize - right_val),
            ast::Infix::Plus => Object::Float(left_val as ast::FloatSize + right_val),
            ast::Infix::Mul => Object::Float(left_val as ast::FloatSize * right_val),
            ast::Infix::Div => Object::Float(left_val as ast::FloatSize / right_val),
            ast::Infix::Percent => Object::Float(left_val as ast::FloatSize % right_val),
            ast::Infix::Exponent => {
                // let num = left_val.pow(right_val as u32);
                // Ok(Object::Float(num as ast::FloatSize))
                Object::Error(EvalError::TypeError("unsupported operand types for ^: 'int' and 'float'".to_string()))
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

        let ast::Ident(name) = ident;
        self.env.set(name, &value);
        None
    }

    fn eval_return_statement(&self, expr: ast::Expression) -> Option<Object> {
        let value = match self.eval_expr(expr) {
            Some(obj) => obj,
            _ => return None
        };

        if Self::is_error(&value) {
            return Some(value);
        }

        Some(Object::Return(Box::new(value)))
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
    use crate::evaluator::error::EvalError;

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
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Float(expected));
        }

        let inputs = vec![
            "4.2 ^ 2",
            "2 ^ 4.3",
        ];

        for input in inputs {
            match test_eval(input).unwrap() {
                Object::Error(EvalError::TypeError(_)) => {},
                _ => assert!(false),
            }
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


    #[test]
    fn return_statement() {
        let expected = vec![
            ("5+5; return 2; 2+2", Object::Int(2)),
            ("return 10;", Object::Int(10)),
            ("let a = 6; let b = a; return a+b; b;", Object::Int(12)),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Some(expected));
        }
    }
}