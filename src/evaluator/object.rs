use std::fmt;
use crate::ast::{IntegerSize, FloatSize, BlockStatement};
use crate::evaluator::error::EvalErrorKind;
use crate::evaluator::Env;
use std::rc::Rc;
use std::cell::RefCell;

pub type EvalResult = Result<Object, EvalErrorKind>;
pub type BuiltinFunc = fn(Vec<Object>) -> EvalResult;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(IntegerSize),
    Float(FloatSize),
    String(String),
    Bool(bool),
    Return(Box<Object>),
    Func(/*params*/Vec<String>, BlockStatement, Rc<RefCell<Env>>),
    Builtin(BuiltinFunc),
    List(Vec<Object>),
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
                let param_list = params.iter().map(|s| format!("{}", s)).collect::<Vec<String>>();
                // let statement_list = body.iter().map(|s| format!("{}", s)).collect::<Vec<String>>();
                // write!(f, "fn ({}) {{\n{}\n}}", param_list.join(", "), statement_list.join(""))
                write!(f, "<fn({})>", param_list.join(", "))
            }
            Object::List(elements) => {
                let element_list = elements.iter().map(|s| format!("{}", s)).collect::<Vec<String>>();
                write!(f, "[{}]", element_list.join(", "))
            }

            Object::Builtin(_func) => {
                write!(f, "<builtin function>")
            }
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