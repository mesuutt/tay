use std::fmt::Formatter;
use std::fmt;
use crate::ast::{Infix, Prefix};
use crate::evaluator::object::Object;

#[derive(Debug, Clone, PartialEq)]
pub enum EvalErrorKind {
    DivideByZero,
    ExponentTooLarge,

    UndefinedIdent(String),
    UnknownPrefixOp(Prefix, Object),
    UnknownInfixOp(Infix, Object, Object),
    UnsupportedInfixOp(Infix, Object, Object),
    TypeMismatch(Infix, Object, Object),
    NotCallable(Object),
}

impl fmt::Display for EvalErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EvalErrorKind::DivideByZero => write!(f, "division by zero"),
            EvalErrorKind::ExponentTooLarge => write!(f, "exponent too large"),
            EvalErrorKind::UndefinedIdent(x) => write!(f, "undefined identifier: '{}'", x),

            EvalErrorKind::UnknownPrefixOp(prefix, o) => write!(f, "unknown prefix: {} {}", prefix, o),
            EvalErrorKind::UnknownInfixOp(infix, l, r) => {
                write!(f, "unknown infix: {} {} {}", l.type_name(), infix, r.type_name())
            },
            EvalErrorKind::UnsupportedInfixOp(infix, l, r) => {
                write!(f, "unsupported operand types for {}: '{}' and '{}'", infix, l.type_name(), r.type_name())
            },
            EvalErrorKind::TypeMismatch(infix, l,r) => {
                write!(f, "type mismatch: {} {} {}", l.type_name(), infix, r.type_name())
            },
            EvalErrorKind::NotCallable(obj) => {
                write!(f, "not callable: {}", obj)
            },

        }
    }
}

