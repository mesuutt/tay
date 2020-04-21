use crate::evaluator::object::{BuiltinFunc, Object, EvalResult, assert_argument_count};
use crate::evaluator::error::EvalErrorKind;

pub struct Builtin {
    name: &'static str,
    builtin: Object
}

pub const BUILTINS: &[Builtin] = &[
    Builtin{
        name: "len",
        builtin: Object::Builtin(len_fn)
    }
];

pub fn lookup_builtin(name: &str) -> Option<Object> {
    for b in BUILTINS {
        if b.name == name {
            return Some(b.builtin.clone())
        }
    }
    None
}

fn len_fn(args: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &args)?;
    match &args[0] {
        Object::String(x) => Ok(Object::Int(x.len() as i64)),
        x => Err(EvalErrorKind::UnsupportedArguments("len", args))
    }
}