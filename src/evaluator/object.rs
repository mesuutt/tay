use std::fmt;
use crate::token::{IntegerSize, FloatSize};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(IntegerSize),
    Float(FloatSize),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{}", x),
            Object::Float(x) => write!(f, "{}", x),
            Object::Error(err) => write!(f, "ERROR: {}", err),
        }
    }
}
