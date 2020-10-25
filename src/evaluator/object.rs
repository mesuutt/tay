use crate::ast::{BlockStatement, FloatSize, IntegerSize};
use crate::evaluator::error::EvalErrorKind;
use crate::evaluator::Env;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::ops::Range;

pub type EvalResult = Result<Object, EvalErrorKind>;
pub type BuiltinFunc = fn(Vec<Object>) -> EvalResult;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(IntegerSize),
    Float(FloatSize),
    String(String),
    Bool(bool),
    Return(Box<Object>),
    Func(
        /*params*/ Vec<String>,
        BlockStatement,
        Rc<RefCell<Env>>,
    ),
    Builtin(BuiltinFunc),
    List(Vec<Object>),
    Hash(HashMap<HashKey, Object>),
    // falsy if expr conditions and assigning variables returns NULL
    // if (1>2) {10} => NULL
    // a = 12 => NULL
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{}", x),
            Object::Float(x) => write!(f, "{}", x),
            Object::String(x) => write!(f, "{}", x),
            Object::Bool(x) => write!(f, "{}", x),
            Object::Func(params, _body, _) => {
                let param_list = params
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>();
                // let statement_list = body.iter().map(|s| format!("{}", s)).collect::<Vec<String>>();
                // write!(f, "fn ({}) {{\n{}\n}}", param_list.join(", "), statement_list.join(""))
                write!(f, "<fn({})>", param_list.join(", "))
            }
            Object::List(elements) => {
                let element_list = elements
                    .iter()
                    .map(|s| format!("{}", s))
                    .collect::<Vec<String>>();
                write!(f, "[{}]", element_list.join(", "))
            }
            Object::Hash(pairs) => {
                let mut element_list = pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>();

                // sorting for testing.
                element_list.sort();
                write!(f, "{{{}}}", element_list.join(", "))
            }
            Object::Builtin(_func) => write!(f, "<builtin function>"),
            Object::Return(x) => write!(f, "{}", x),
            Object::Null => write!(f, ""),
        }
    }
}

impl Object {
    pub fn type_name(&self) -> &str {
        match self {
            Object::String(_) => "STRING",
            Object::Int(_) => "INTEGER",
            Object::Float(_) => "FLOAT",
            Object::Bool(_) => "BOOL",
            Object::Return(_) => "RETURN",
            Object::Null => "NULL",
            Object::Func(_, _, _) => "FUNC",
            Object::Builtin(_) => "BUILTIN",
            Object::List(_) => "LIST",
            Object::Hash(_) => "HASH",
        }
    }
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum HashKey {
    String(String),
    Int(IntegerSize),
    Bool(bool),
}

impl HashKey {
    pub fn from_object(obj: &Object) -> Result<HashKey, EvalErrorKind> {
        match obj {
            Object::Int(v) => Ok(HashKey::Int(*v)),
            Object::Bool(v) => Ok(HashKey::Bool(*v)),
            Object::String(v) => Ok(HashKey::String(v.clone())),
            _ => Err(EvalErrorKind::UnsupportedHashKey(obj.clone())),
        }
    }
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HashKey::Int(v) => write!(f, "{}", v),
            HashKey::String(v) => write!(f, "\"{}\"", v),
            HashKey::Bool(v) => write!(f, "{}", v),
        }
    }
}

pub fn assert_argument_count(expected: usize, args: &[Object]) -> Result<(), EvalErrorKind> {
    if args.len() != expected {
        Err(EvalErrorKind::WrongArgumentCount(expected, args.len()))
    } else {
        Ok(())
    }
}

pub fn assert_argument_count_range(expected: Range<usize>, args: &[Object]) -> Result<(), EvalErrorKind> {
    let arg_len = args.len();
    if arg_len < expected.start || arg_len > expected.end {
        Err(EvalErrorKind::WrongArgumentCount(expected.start, args.len()))
    } else {
        Ok(())
    }
}
