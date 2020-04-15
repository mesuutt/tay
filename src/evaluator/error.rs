use std::fmt::Formatter;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum EvalErrorKind {
    DivideByZero,
    ExponentTooLarge,
    UndefinedIdent(String),
    TypeError(String),
    EvaluationError(String),
}

impl fmt::Display for EvalErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EvalErrorKind::DivideByZero => write!(f, "Division by zero"),
            EvalErrorKind::ExponentTooLarge => write!(f, "exponent too large"),
            EvalErrorKind::UndefinedIdent(x) => write!(f, "undefined identifier: '{}'", x),
            EvalErrorKind::TypeError(x) => write!(f, "TypeError: {}", x),
            EvalErrorKind::EvaluationError(x) => write!(f, "EvaluationError: {}", x),
        }
    }
}

