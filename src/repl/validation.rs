use rustyline::error::ReadlineError;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};

pub struct InputValidator {}
/*
impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult, ReadlineError> {
        let input = ctx.input();
        if !input.starts_with("SELECT") {
            return Ok(ValidationResult::Invalid(Some(
                " --< Expect: SELECT stmt".to_owned(),
            )));
        } else if !input.ends_with(';') {
            return Ok(ValidationResult::Incomplete);
        }
        Ok(ValidationResult::Valid(None))
    }
}*/

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult, ReadlineError> {
        Ok(validate_brackets(ctx.input()))
    }
}

fn validate_brackets(input: &str) -> ValidationResult {
    let mut stack = vec![];
    for c in input.chars() {
        match c {
            '(' | '[' | '{' => stack.push(c),
            ')' | ']' | '}' => match (stack.pop(), c) {
                (Some('('), ')') | (Some('['), ']') | (Some('{'), '}') => {}
                (Some(wanted), _) => {
                    return ValidationResult::Invalid(Some(format!(
                        "Mismatched brackets: {:?} is not properly closed",
                        wanted
                    )))
                }
                (None, c) => {
                    return ValidationResult::Invalid(Some(format!(
                        "Mismatched brackets: {:?} is unpaired",
                        c
                    )))
                }
            },
            _ => {}
        }
    }
    if stack.is_empty() {
        ValidationResult::Valid(None)
    } else {
        ValidationResult::Incomplete
    }
}