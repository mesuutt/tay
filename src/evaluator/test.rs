#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::evaluator::{Env, eval};
    use super::super::{Evaluator, Object};
    use crate::evaluator::error::EvalErrorKind;
    use crate::ast;
    use crate::ast::BlockStatement;
    use std::rc::Rc;
    use std::cell::RefCell;
    use pretty_assertions::assert_eq;
    use crate::evaluator::object::EvalResult;

    fn test_eval(input: &str) -> EvalResult {
        let prog = Parser::new(Lexer::new(input)).parse();
        let env = Rc::new(RefCell::new(Env::new()));
        eval(prog, env)
    }

    #[test]
    fn integer_expr() {
        let expected = vec![
            ("5", 5),
            ("10", 10),
            ("-10", -10),
            ("5 + 5 - 5", 5),
            ("(5 + 5) * 5", 50),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Int(expected));
        }
    }

    #[test]
    fn float_expr() {
        let expected = vec![
            ("5.1", 5.1),
            ("5.2 * 5.1", 26.52),
            ("2 * 4.2", 8.4),
            ("4.2 * 2", 8.4),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Ok(Object::Float(expected)));
        }

        let inputs = vec![
            "4.2 ^ 2",
            "2 ^ 4.3",
        ];

        for input in inputs {
            let evaluated = test_eval(input);
            match evaluated {
                Err(EvalErrorKind::UnsupportedInfixOp(_, _, _)) => {}
                _ => assert!(false),
            }
        }
    }

    #[test]
    fn let_statement() {
        let expected = vec![
            ("let x = 5; x;", 5),
            ("let x = 5 * 5; x;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Ok(Object::Int(expected)));
        }
    }

    #[test]
    fn return_statement() {
        let expected = vec![
            ("5+5; return 2; 2+2", Object::Int(2)),
            ("return 10;", Object::Int(10)),
            ("let a = 6; let b = a; return a+b; b;", Object::Int(12)),
        ];

        for (input, expected) in expected {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Ok(expected));
        }
    }

    #[test]
    fn function() {
        let evaluated = test_eval("fn(x) {x+2}");

        if let Ok(Object::Func(params, statements, _)) = evaluated {
            assert_eq!(params.len(), 1);
            assert_eq!(params.iter().map(|i| format!("{}", i)).collect::<Vec<String>>().join(", "), "x");
            assert_eq!(statements.iter().map(|s| format!("{}", s)).collect::<Vec<String>>().join(""), "(x + 2)");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn apply_func() {
        let test_data = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5_i64),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in test_data {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Ok(Object::Int(expected)))
        }
    }

    #[test]
    fn if_else() {
        let test_data = vec![
            ("if (true) { 10 }", Object::Int(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Int(10)),
            ("if (1 < 2) { 10 }", Object::Int(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Int(10)),
        ];

        for (input, expected) in test_data {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Ok(expected))
        }
    }

    #[test]
    fn string_literal() {
        let test_data = vec![
            (r#""hello world""#, Object::String("hello world".to_owned())),
            (r#""hello" + " " +  "world" "#, Object::String("hello world".to_owned())),
        ];

        for (input, expected) in test_data {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Ok(expected))
        }
    }
}