use std::fmt::Formatter;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    DivideByZero,
    ExponentTooLarge,
    UndefinedIdent(String)
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::DivideByZero => write!(f, "Division by zero"),
            EvalError::ExponentTooLarge => write!(f, "exponent too large"),
            EvalError::UndefinedIdent(x) => write!(f, "undefined identifier: '{}'", x)
        }
    }
}

