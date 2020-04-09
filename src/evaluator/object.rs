use std::fmt;
use crate::ast::{IntegerSize, FloatSize};
use crate::evaluator::error::EvalError;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(IntegerSize),
    Float(FloatSize),
    Bool(bool),
    Error(EvalError),
    Return(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{}", x),
            Object::Float(x) => write!(f, "{}", x),
            Object::Bool(x) => write!(f, "{}", x),
            Object::Error(err) => write!(f, "ERROR: {}", err),
            Object::Return(x) => write!(f, "{}", x),
        }
    }
}
