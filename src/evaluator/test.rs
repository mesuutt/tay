
#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::evaluator::Env;
    use crate::evaluator::{Evaluator, Object};
    use crate::evaluator::error::EvalError;
    use crate::ast;
    use crate::ast::BlockStatement;
    use std::rc::Rc;
    use std::cell::RefCell;

    fn test_eval(input: &str) -> Option<Object> {
        let prog = Parser::new(Lexer::new(input)).parse();
        let env = Env::new();
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(env)));
        evaluator.eval(prog)
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
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, Object::Float(expected));
        }

        let inputs = vec![
            "4.2 ^ 2",
            "2 ^ 4.3",
        ];

        for input in inputs {
            match test_eval(input).unwrap() {
                Object::Error(EvalError::TypeError(_)) => {}
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
            assert_eq!(evaluated, Some(Object::Int(expected)));
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
            assert_eq!(evaluated, Some(expected));
        }
    }

    #[test]
    fn function() {
        let evaluated = test_eval("fn(x) {x+2}");

        if let Some(Object::Func(params, statements, _)) = evaluated {
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
            assert_eq!(evaluated, Some(Object::Int(expected)))
        }
    }
}