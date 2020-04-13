use std::fmt;
use crate::ast::{IntegerSize, FloatSize, Ident, BlockStatement};
use crate::evaluator::error::EvalError;
use crate::evaluator::Env;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(IntegerSize),
    Float(FloatSize),
    Bool(bool),
    Error(EvalError),
    Return(Box<Object>),
    Func(/*params*/Vec<Ident>, BlockStatement, Rc<RefCell<Env>>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{}", x),
            Object::Float(x) => write!(f, "{}", x),
            Object::Bool(x) => write!(f, "{}", x),
            Object::Func(params, body, _) => {
                let param_list = params.iter().map(|s| format!("{}", s)).collect::<Vec<String>>();
                let statement_list = body.iter().map(|s| format!("{}", s)).collect::<Vec<String>>();
                write!(f, "fn ({}) {{\n{}\n}}", param_list.join(", "), statement_list.join(""))
                //write!(f, "{}", "func fixme")
            },
            Object::Return(x) => write!(f, "{}", x),
            Object::Error(err) => write!(f, "{}", err),
        }
    }
}
