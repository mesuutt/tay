use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(int) => write!(f, "{}", int),
            Object::Error(err) => write!(f, "ERROR: {}", err),
        }
    }
}
