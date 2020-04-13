mod env;
mod object;
mod error;

pub use env::Env;
use object::Object;
use crate::ast;
use crate::evaluator::error::EvalError;
use crate::ast::{FloatSize, Expression};
use std::rc::Rc;
use std::cell::RefCell;

pub struct Evaluator {
    env: Rc<RefCell<Env>>,  // Without RefCell we can't borrow Env as mutable
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Env>>) -> Evaluator {
        Evaluator {
            env: env,
        }
    }

    pub fn eval(&mut self, prog: ast::Program) -> Option<Object> {
        let mut result = None;
        for s in prog.statements {
            match self.eval_statement(&s) {
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                Some(Object::Return(val)) => return Some(*val),
                obj => {
                    result = obj;
                }
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: &ast::Statement) -> Option<Object> {
        match statement {
            ast::Statement::Expression(expr) => self.eval_expr(expr),
            ast::Statement::Let(ident, expr) => self.eval_let_statement(ident, expr),
            ast::Statement::Return(expr) => self.eval_return_statement(expr),
        }
    }

    fn eval_block_statement(&mut self, statement: &ast::BlockStatement) -> Option<Object> {
        let mut result = None;
        for s in statement {
            match self.eval_statement(s) {
                Some(Object::Return(value)) => return Some(Object::Return(value)),
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                obj => {
                    result = obj
                }
            }
        }
        result
    }

    fn eval_expr(&mut self, expr: &ast::Expression) -> Option<Object> {
        match expr {
            ast::Expression::Literal(lit) => Some(self.eval_literal(lit)),
            ast::Expression::Ident(ident) => Some(self.eval_ident(ident)),
            ast::Expression::Prefix(operator, expr) => {
                if let Some(right) = self.eval_expr(&*expr) {
                    Some(self.eval_prefix_expr(operator, right))
                } else {
                    None
                }
            }
            ast::Expression::Infix(operator, left_expr, right_expr) => {
                let left = self.eval_expr(&*left_expr);
                let right = self.eval_expr(&*right_expr);
                match (&left, &right) {
                    (Some(Object::Error(_)), _) => left,
                    (_, Some(Object::Error(_))) => right,
                    _ => self.eval_infix_expr(operator, left.unwrap(), right.unwrap())
                }
            }
            ast::Expression::Func { params, body } => {
                Some(Object::Func(params.to_vec(), body.to_vec(), self.env.clone()))
            }
            ast::Expression::Call { func, args } => {
                Some(self.eval_call_expr(func, args))
            }
            _ => None
        }
    }

    fn eval_call_expr(&mut self, func_expr: &Expression, call_with_args: &[Expression]) -> Object {
        let args = match self.eval_expressions(call_with_args) {
            Some(object_list) => {
                if object_list.len() == 1 && Self::is_error(object_list.first().unwrap()) {
                    return object_list.first().unwrap().clone();
                }
                object_list
            }
            None => {
                return Object::Error(EvalError::EvaluationError("call exprresion cannot evaluated".to_owned()));
            }
        };

        let func_obj = self.eval_expr(&*func_expr);
        match &func_obj {
            Some(Object::Func(params, _body, _env)) => {
                if params.len() != call_with_args.len() {
                    return Object::Error(EvalError::EvaluationError(format!("'{}' function takes {} positional arguments but {} were given", func_expr, params.len(), call_with_args.len())));
                }
            }
            Some(Object::Error(e)) => return Object::Error(e.to_owned()),
            _ => {}
        }

        self.apply_func(&func_obj.unwrap(), &args)
    }


    fn eval_expressions(&mut self, exprs: &[Expression]) -> Option<Vec<Object>> {
        let mut result = Vec::<Object>::new();
        for e in exprs {
            let evaluated = match self.eval_expr(&e) {
                Some(expr) => {
                    if Self::is_error(&expr) {
                        result.push(expr);
                        return Some(result);
                    }
                    expr
                }
                None => {
                    result.push(Object::Error(EvalError::EvaluationError(format!("{} cannot evaluated", e))));
                    return Some(result);
                }
            };
            result.push(evaluated)
        }

        Some(result)
    }

    fn eval_literal(&self, literal: &ast::Literal) -> Object {
        match literal {
            ast::Literal::Int(x) => Object::Int(*x),
            ast::Literal::Float(x) => Object::Float(*x),
            ast::Literal::Bool(x) => Object::Bool(*x),
        }
    }

    fn eval_ident(&self, ident: &ast::Ident) -> Object {
        let ast::Ident(name) = ident;

        // If we use borrow() instead borrow_mut() value removing after return.
        match self.env.borrow_mut().get(&name) {
            Some(val) => val,
            None => Object::Error(EvalError::UndefinedIdent(name.clone()))
        }
    }

    fn eval_prefix_expr(&self, operator: &ast::Prefix, right: Object) -> Object {
        match operator {
            ast::Prefix::Minus => self.eval_minus_prefix_operator_expr(right),
            ast::Prefix::Bang => self.eval_bang_operator_expr(right),
        }
    }

    fn eval_infix_expr(&self, operator: &ast::Infix, left: Object, right: Object) -> Option<Object> {
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

    fn eval_minus_prefix_operator_expr(&self, right: Object) -> Object {
        match right {
            Object::Int(x) => Object::Int(-x),
            _ => Object::Error(EvalError::TypeError(format!("unknown operator: '-{}'", right)))
        }
    }

    fn eval_bang_operator_expr(&self, right: Object) -> Object {
        match right {
            Object::Bool(true) => Object::Bool(false),
            Object::Bool(false) => Object::Bool(true),
            _ => Object::Bool(false),
        }
    }

    fn eval_integer_infix_expr(&self, operator: &ast::Infix, left_val: i64, right_val: i64) -> Object {
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
            ast::Infix::Lt => {
                if left_val < right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Gt => {
                if left_val > right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Lte => {
                if left_val <= right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Gte => {
                if left_val >= right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Eq => {
                if left_val == right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::NotEq => {
                if left_val != right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
        }
    }

    fn eval_float_infix_expr(&self, operator: &ast::Infix, left_val: ast::FloatSize, right_val: ast::FloatSize) -> Object {
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
            ast::Infix::Lt => {
                if (left_val as FloatSize) < right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Gt => {
                if (left_val as FloatSize) > right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Lte => {
                if (left_val as FloatSize) <= right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Gte => {
                if (left_val as FloatSize) >= right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Eq => {
                if ((left_val as FloatSize) - right_val).abs() == 0.0 {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::NotEq => {
                if ((left_val as FloatSize) - right_val).abs() != 0.0 {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
        }
    }

    fn eval_float_with_int_infix_expr(&self, operator: &ast::Infix, left_val: ast::FloatSize, right_val: ast::IntegerSize) -> Object {
        match operator {
            ast::Infix::Minus => Object::Float(left_val - right_val as ast::FloatSize),
            ast::Infix::Plus => Object::Float(left_val + right_val as ast::FloatSize),
            ast::Infix::Mul => Object::Float(left_val * right_val as ast::FloatSize),
            ast::Infix::Div => Object::Float(left_val / right_val as ast::FloatSize),
            ast::Infix::Percent => Object::Float(left_val % right_val as ast::FloatSize),
            ast::Infix::Exponent => {
                // let num = left_val.powi(right_val as i32);
                // Ok(Object::Float(num))
                Object::Error(EvalError::TypeError("unsupported operand types for ^: 'float' and 'int'".to_owned()))
            }
            ast::Infix::Lt => {
                if left_val < (right_val as FloatSize) {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Gt => {
                if left_val > (right_val as FloatSize) {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Lte => {
                if left_val <= (right_val as FloatSize) {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Gte => {
                if left_val >= (right_val as FloatSize) {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Eq => {
                if (left_val - (right_val as FloatSize)).abs() == 0.0 {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::NotEq => {
                if (left_val - (right_val as FloatSize)).abs() != 0.0 {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
        }
    }

    fn eval_int_with_float_infix_expr(&self, operator: &ast::Infix, left_val: ast::IntegerSize, right_val: ast::FloatSize) -> Object {
        match operator {
            ast::Infix::Minus => Object::Float(left_val as ast::FloatSize - right_val),
            ast::Infix::Plus => Object::Float(left_val as ast::FloatSize + right_val),
            ast::Infix::Mul => Object::Float(left_val as ast::FloatSize * right_val),
            ast::Infix::Div => Object::Float(left_val as ast::FloatSize / right_val),
            ast::Infix::Percent => Object::Float(left_val as ast::FloatSize % right_val),
            ast::Infix::Exponent => {
                // let num = left_val.pow(right_val as u32);
                // Ok(Object::Float(num as ast::FloatSize))
                Object::Error(EvalError::TypeError("unsupported operand types for ^: 'int' and 'float'".to_owned()))
            }
            ast::Infix::Lt => {
                if (left_val as FloatSize) < right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Gt => {
                if (left_val as FloatSize) > right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Lte => {
                if (left_val as FloatSize) <= right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Gte => {
                if (left_val as FloatSize) >= right_val {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::Eq => {
                if ((left_val as FloatSize) - right_val).abs() == 0.0 {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
            ast::Infix::NotEq => {
                if (right_val - (left_val as FloatSize)).abs() != 0.0 {
                    Object::Bool(true)
                } else {
                    Object::Bool(false)
                }
            }
        }
    }

    fn eval_let_statement(&mut self, ident: &ast::Ident, expr: &ast::Expression) -> Option<Object> {
        let value = match self.eval_expr(expr) {
            Some(val) => val,
            None => return None
        };

        if Self::is_error(&value) {
            return Some(value);
        }

        let ast::Ident(name) = ident;
        self.env.borrow_mut().set(name.clone(), &value);
        None
    }

    fn eval_return_statement(&mut self, expr: &ast::Expression) -> Option<Object> {
        let value = match self.eval_expr(&expr) {
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


    fn apply_func(&mut self, func: &Object, args: &[Object]) -> Object {
        let current_env = self.env.clone();
        if let Object::Func(_params, body, _) = func {
            let extended_env = Self::extend_func_env(func, args);

            // Switching scope before evaluate block
            self.env = Rc::new(RefCell::new(extended_env));

            let obj = match self.eval_block_statement(body) {
                Some(Object::Return(obj)) => *obj,
                Some(o) => o, // Evaluated expression result without return statement.
                None => Object::Error(EvalError::EvaluationError("apply func error".to_owned()))
            };
            self.env = current_env;

            obj
        } else {
            Object::Error(EvalError::EvaluationError("invalid func literal".to_owned()))
        }
    }

    fn extend_func_env(func: &Object, call_args: &[Object]) -> Env {
        // Add function parameters to env of function
        if let Object::Func(params, _, outer_env) = func {
            let mut env = Env::new_with_outer(&outer_env);

            for (i, param) in params.iter().enumerate() {
                env.set(param.0.clone(), call_args.get(i).unwrap())
            }
            return env;
        }

        Env::new() // FIXME: this line should not be run
    }
}


#[cfg(test)]
mod test {
    use super::Env;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use super::{Evaluator, Object};
    use crate::evaluator::error::EvalError;
    use crate::ast;
    use crate::ast::BlockStatement;
    use std::rc::Rc;
    use std::cell::RefCell;

    fn test_eval(input: &str) -> Option<Object> {
        let prog = Parser::new(Lexer::new(input)).parse();
        let env = Env::new();
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(env)));
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
                Object::Error(EvalError::TypeError(_)) => {}
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

    #[test]
    fn function() {
        let evaluated = test_eval("fn(x) {x+2}");

        if let Some(Object::Func(params, statements, _)) = evaluated {
            assert_eq!(params.len(), 1);
            assert_eq!(params.iter().map(|i| format!("{}", i)).collect::<Vec<String>>().join(", "), "x");
            assert_eq!(statements.iter().map(|s| format!("{}", s)).collect::<Vec<String>>().join(""), "(x + 2)");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn apply_func() {
        let test_data = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5_i64),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in test_data {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Some(Object::Int(expected)))
        }
    }
}