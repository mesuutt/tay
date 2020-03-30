use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    Int(i64),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(int) => write!(f, "{}", int),
            Object::Null => write!(f, "null"),
            Object::Error(err) => write!(f, "ERROR: {}", err),
        }
    }
}
