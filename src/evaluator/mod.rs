mod env;
mod object;
mod error;
mod builtins;

#[cfg(test)]
mod test;

pub use env::Env;
pub use object::Object;
use crate::ast;
use crate::evaluator::error::EvalErrorKind;
use crate::ast::{FloatSize, Expression, BlockStatement, Program, Statement, Infix, Prefix, Ident};
use std::rc::Rc;
use std::cell::RefCell;
use crate::evaluator::object::EvalResult;
use crate::evaluator::builtins::{lookup_builtin};


pub fn eval(program: Program, env: Rc<RefCell<Env>>) -> EvalResult {
    let mut result = Object::Null;
    for s in program.statements {
        result = eval_statement(&s, Rc::clone(&env))?;
        if let Object::Return(value) = result {
            return Ok(*value);  // Stop evaluation
        }
    }

    Ok(result)
}

fn eval_statement(statement: &Statement, env: Rc<RefCell<Env>>) -> EvalResult {
    match statement {
        Statement::Expression(expr) => eval_expr(expr, env),
        Statement::Let(ident, expr) => eval_let_statement(ident, expr, env),
        Statement::Return(expr) => eval_return_statement(expr, env),
    }
}

fn eval_block_statement(statement: &BlockStatement, env: Rc<RefCell<Env>>) -> EvalResult {
    let mut result = Object::Null;
    for s in statement {
        result = eval_statement(&s, Rc::clone(&env))?;
        if let Object::Return(value) = result {
            return Ok(Object::Return(value)); // Stop evaluation
        }
    }

    Ok(result)
}


fn eval_expr(expr: &Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    match expr {
        Expression::Literal(lit) => Ok(eval_literal(lit)),
        Expression::Ident(ident) => eval_ident(ident, env),
        Expression::Prefix(operator, expr) => {
            eval_prefix_expr(operator, expr, env)
        }
        Expression::Infix(operator, left_expr, right_expr) => {
            eval_infix_expr(operator, left_expr, right_expr, env)
        }
        ast::Expression::Func {identifier, params, body } => {
            let func = Object::Func(params.to_vec(), body.clone(), env.clone());
            if identifier.is_some() { // if function has a name, add to env
                let ast::Ident(key) = identifier.as_ref().unwrap();
                env.borrow_mut().set(key.clone(), &func);
            }

            Ok(func)
        }
        ast::Expression::Call { func, args } => {
            eval_call_expr(func, args, env)
        }
        ast::Expression::If { condition, consequence, alternative } => {
            eval_if_expression(condition, consequence, alternative, env)
        }
    }
}

fn eval_exprs(exprs: &[Expression], env: Rc<RefCell<Env>>) -> Result<Vec<Object>, EvalErrorKind> {
    let mut result = Vec::<Object>::new();
    for e in exprs {
        result.push(eval_expr(&e, env.clone())?)
    }
    Ok(result)
}

fn eval_prefix_expr(operator: &Prefix, right_expr: &Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    let right = eval_expr(right_expr, env)?;
    match operator {
        Prefix::Minus => {
            match right {
                Object::Int(x) => Ok(Object::Int(-x)),
                _ => Err(EvalErrorKind::UnknownPrefixOp(Prefix::Minus, right))
            }
        },
        Prefix::Bang => {
            Ok(match right {
                Object::Bool(true) => Object::Bool(false),
                Object::Bool(false) => Object::Bool(true),
                Object::Null => Object::Bool(true),
                _ => Object::Bool(false),
            })
        }
    }
}

fn eval_infix_expr(operator: &Infix, left_expr: &Expression, right_expr: &Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    let left_obj = eval_expr(left_expr, Rc::clone(&env))?;
    let right_obj = eval_expr(right_expr, env)?;
    match (left_obj, right_obj) {
        (Object::Int(x), Object::Int(y)) => {
            eval_integer_infix_expr(operator, x, y)
        }
        (Object::Float(x), Object::Float(y)) => {
            eval_float_infix_expr(operator, x, y)
        }
        (Object::Float(x), Object::Int(y)) => {
            eval_float_with_int_infix_expr(operator, x, y)
        }
        (Object::Int(x), Object::Float(y)) => {
            eval_int_with_float_infix_expr(operator, x, y)
        }
        (Object::String(x), Object::String(y)) => {
            eval_string_infix_expr(operator, &x, &y)
        }
        (l, r) => Err(EvalErrorKind::TypeMismatch(operator.clone(), l, r))
    }
}


fn eval_if_expression(
    cond: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
    env: Rc<RefCell<Env>>,
) -> EvalResult {
    let cond = eval_expr(cond, env.clone())?;
    if is_truthy(&cond) {
        eval_block_statement(consequence, env)
    } else if let Some(alt) = alternative {
        eval_block_statement(alt.as_ref(), env)
    } else {
        Ok(Object::Null)
    }
}


fn eval_literal(literal: &ast::Literal) -> Object {
    match literal {
        ast::Literal::Int(x) => Object::Int(*x),
        ast::Literal::Float(x) => Object::Float(*x),
        ast::Literal::Bool(x) => Object::Bool(*x),
        ast::Literal::String(x) => Object::String(x.clone()),
    }
}

fn eval_ident(ident: &ast::Ident, env: Rc<RefCell<Env>>) -> EvalResult {
    let ast::Ident(name) = ident;
    // If we use borrow() instead borrow_mut() value removing after return.
    match env.borrow_mut().get(&name) {
        Some(val) => Ok(val),
        None => {
            if let Some(builtin) =  lookup_builtin(name) {
                Ok(builtin)
            } else {
                Err(EvalErrorKind::UndefinedIdent(name.clone()))
            }
        }
    }
}

fn eval_integer_infix_expr(operator: &ast::Infix, left_val: i64, right_val: i64) -> EvalResult {
    let result = match operator {
        ast::Infix::Minus => Object::Int(left_val - right_val),
        ast::Infix::Plus => Object::Int(left_val + right_val),
        ast::Infix::Mul => Object::Int(left_val * right_val),
        ast::Infix::Div => {
            if right_val == 0 {
                return Err(EvalErrorKind::DivideByZero);
            }
            Object::Int(left_val / right_val)
        }
        ast::Infix::Percent => Object::Int(left_val % right_val),
        ast::Infix::Exponent => {
            let (num, is_overflow) = (left_val as i32).overflowing_pow(right_val as u32);
            if is_overflow {
                return Err(EvalErrorKind::ExponentTooLarge)
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
    };

    Ok(result)
}

fn eval_float_infix_expr(operator: &ast::Infix, left_val: ast::FloatSize, right_val: ast::FloatSize) -> EvalResult {
    let result = match operator {
        ast::Infix::Minus => Object::Float(left_val - right_val),
        ast::Infix::Plus => Object::Float(left_val + right_val),
        ast::Infix::Mul => Object::Float(left_val * right_val),
        ast::Infix::Div => {
            if right_val == 0.0 {
                return Err(EvalErrorKind::DivideByZero);
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
    };

    Ok(result)
}

fn eval_float_with_int_infix_expr(operator: &ast::Infix, left_val: ast::FloatSize, right_val: ast::IntegerSize) -> EvalResult {
    let result = match operator {
        ast::Infix::Minus => Object::Float(left_val - right_val as ast::FloatSize),
        ast::Infix::Plus => Object::Float(left_val + right_val as ast::FloatSize),
        ast::Infix::Mul => Object::Float(left_val * right_val as ast::FloatSize),
        ast::Infix::Div => Object::Float(left_val / right_val as ast::FloatSize),
        ast::Infix::Percent => Object::Float(left_val % right_val as ast::FloatSize),
        ast::Infix::Exponent => {
            // let num = left_val.powi(right_val as i32);
            // Ok(Object::Float(num))
            return Err(EvalErrorKind::UnsupportedInfixOp(
                operator.clone(), Object::Float(left_val), Object::Int(right_val))
            )
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
    };

    Ok(result)
}

fn eval_int_with_float_infix_expr(operator: &ast::Infix, left_val: ast::IntegerSize, right_val: ast::FloatSize) -> EvalResult {
    let result = match operator {
        ast::Infix::Minus => Object::Float(left_val as ast::FloatSize - right_val),
        ast::Infix::Plus => Object::Float(left_val as ast::FloatSize + right_val),
        ast::Infix::Mul => Object::Float(left_val as ast::FloatSize * right_val),
        ast::Infix::Div => Object::Float(left_val as ast::FloatSize / right_val),
        ast::Infix::Percent => Object::Float(left_val as ast::FloatSize % right_val),
        ast::Infix::Exponent => {
            // let num = left_val.pow(right_val as u32);
            // Ok(Object::Float(num as ast::FloatSize))
            return Err(EvalErrorKind::UnsupportedInfixOp(
                operator.clone(), Object::Int(left_val), Object::Float(right_val))
            )
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
    };

    Ok(result)
}

fn eval_string_infix_expr(operator: &ast::Infix, left_val: &str, right_val: &str) -> EvalResult {
    let result = match operator {
        ast::Infix::Plus => {
            let mut s = left_val.to_owned();
            s.push_str(&right_val.to_owned());
            Object::String(s)
        }
        ast::Infix::Eq => {
            Object::Bool(left_val == right_val)
        }
        ast::Infix::NotEq => {
            Object::Bool(left_val != right_val)
        }
        _op => {
            return Err(EvalErrorKind::UnknownInfixOp(
                operator.clone(),
                Object::String(left_val.to_owned()),
                Object::String(right_val.to_owned())
            ))
        }
    };

    Ok(result)
}

fn eval_let_statement(ident: &ast::Ident, expr: &ast::Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    let value = eval_expr(expr, env.clone())?;
    let ast::Ident(name) = ident;
    env.borrow_mut().set(name.clone(), &value);
    Ok(Object::Null)
}

fn eval_return_statement(expr: &ast::Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    let value = eval_expr(&expr, env)?;
    Ok(Object::Return(Box::new(value)))
}

fn eval_call_expr(func_expr: &Expression, arguments: &[Expression], env: Rc<RefCell<Env>>) -> EvalResult {
    let args = eval_exprs(arguments, env.clone())?;
    let func = eval_expr(&*func_expr, env.clone())?;
    apply_func(&func, &args, env)
}

fn apply_func(func: &Object, args: &[Object], env: Rc<RefCell<Env>>) -> EvalResult {
    match func {
        Object::Func(params, body, _) => {
            let extended_env = extend_func_env(params.clone(), args, env.clone());
            let evaluated = eval_block_statement(body, extended_env)?;
            match evaluated {
                Object::Return(val) => Ok(*val),
                _ => Ok(evaluated)
            }
        }
        Object::Builtin(func) => {
            // TODO: Make builtings runnable with refs instead values.
            // for example: len(['a', 'big', 'list'])
            func(args.to_vec())
        },
        _ => Err(EvalErrorKind::NotCallable(func.clone()))
    }
}

fn extend_func_env(params: Vec<Ident>, args: &[Object], parent_env: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
    let new_env = Rc::new(RefCell::new(Env::extend(&parent_env)));
    for (i, param) in params.iter().enumerate() {
        new_env.borrow_mut().set(param.0.clone(), args.get(i).unwrap())
    }
    new_env
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Bool(false) => false,
        Object::Bool(true) => true,
        _ => true // other expressions. Maybe we dont allow this in the future: if (1){...}
    }
}