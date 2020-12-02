use crate::evaluator::object::{Object, EvalResult, assert_argument_count, assert_argument_count_range};
use crate::evaluator::error::EvalErrorKind;
use crate::evaluator::is_truthy;

pub struct Builtin {
    name: &'static str,
    builtin: Object
}

pub const BUILTINS: &[Builtin] = &[
    Builtin{
        name: "len",
        builtin: Object::Builtin(len_fn)
    },
    Builtin{
        name: "print",
        builtin: Object::Builtin(print_fn)
    },
    Builtin{
        name: "println",
        builtin: Object::Builtin(println_fn)
    },
    Builtin{
        name: "assert",
        builtin: Object::Builtin(assert_fn)
    },
    Builtin{
        name: "assert_eq",
        builtin: Object::Builtin(assert_eq_fn)
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
        Object::List(elems) => Ok(Object::Int(elems.len() as i64)),
        _ => Err(EvalErrorKind::UnsupportedArguments("len", args))
    }
}

fn print_fn(args: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &args)?;
    for i in args {
        print!("{}", i)
    }
    Ok(Object::Null)
}

fn println_fn(args: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &args)?;
    println!("{}", args[0]);
    Ok(Object::Null)
}

fn assert_fn(args: Vec<Object>) -> EvalResult {
    assert_argument_count_range(1..2, &args)?;
    if !is_truthy(&args[0].clone()) {
        if args.len() == 2 {
            Err(EvalErrorKind::AssertionError(args[1].clone()))
        } else {
            Err(EvalErrorKind::AssertionError(Object::String("".to_owned())))
        }
    } else {
        Ok(Object::Null)
    }
}

fn assert_eq_fn(args: Vec<Object>) -> EvalResult {
    assert_argument_count_range(2..3, &args)?;
    if args[0] != args[1] {
        if args.len() == 3 {
            Err(EvalErrorKind::AssertionError(Object::String(format!("{}", args.last().unwrap().to_owned()))))
        } else {
            Err(EvalErrorKind::AssertionError(Object::String("".to_owned())))
        }
    } else {
        Ok(Object::Null)
    }

}