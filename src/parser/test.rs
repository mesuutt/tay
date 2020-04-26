#[cfg(test)]
pub mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast::{Statement, Literal, Expression, Prefix, Infix};
    use pretty_assertions::assert_eq;

    #[test]
    fn let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;
        let z = 1.2;
        let foo = "hello";
        "#;

        let mut p = Parser::new(Lexer::new(input.to_owned()));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 4);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Let(String::from("x"), Expression::Literal(Literal::Int(5))),
            Statement::Let(String::from("y"), Expression::Literal(Literal::Int(10))),
            Statement::Let(String::from("z"), Expression::Literal(Literal::Float(1.2))),
            Statement::Let(String::from("foo"), Expression::Literal(Literal::String("hello".to_owned()))),
        ]);
    }


    #[test]
    fn return_statement() {
        let input = r#"
        return 1;
        return foo;
        "#;

        let mut p = Parser::new(Lexer::new(input.to_owned()));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 2);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Return(Expression::Literal(Literal::Int(1))),
            Statement::Return(Expression::Ident("foo".to_string())),
        ]);
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) {x} else {y}";
        let mut p = Parser::new(Lexer::new(input.to_owned()));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 1);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Expression(Expression::If {
                condition: Box::new(
                    Expression::Infix(
                        Infix::Lt,
                        Box::new(Expression::Ident("x".to_string())),
                        Box::new(Expression::Ident("y".to_string())),
                    )),
                consequence: vec![Statement::Expression(Expression::Ident("x".to_string()))],
                alternative: Some(vec![Statement::Expression(Expression::Ident("y".to_string()))]),
            }),
        ]);
    }


    #[test]
    fn function() {
        let expected = vec![
            ("fn () {}", None, Vec::<&str>::new(), Vec::<&str>::new()),
            ("fn (x, y) {x * y}", None, vec!["x", "y"], vec!["(x * y)"]),
            ("fn (x, y, z) {x * y * z}", None, vec!["x", "y", "z"], vec!["((x * y) * z)"]),
            ("fn foo() {}", Some("foo".to_owned()), Vec::<&str>::new(), Vec::<&str>::new()),
        ];

        for (input, expect_name, expect_param, expect_body) in expected {
            let mut p = Parser::new(Lexer::new(input.to_owned()));
            let program = p.parse();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.errors.len(), 0);

            if let Statement::Expression(Expression::Func { identifier, params, body }) = program.statements.first().unwrap() {
                assert_eq!(*identifier, expect_name);
                assert_eq!(params.iter().map(|s| format!("{}", s)).collect::<Vec<String>>(), expect_param);
                assert_eq!(body.iter().map(|s| format!("{}", s)).collect::<Vec<String>>(), expect_body)
            } else {
                assert!(false)
            }
        }
    }


    #[test]
    fn call_expression() {
        let program = Parser::new(Lexer::new("add(1, 2)".to_owned())).parse();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.errors.len(), 0);
        if let Statement::Expression(Expression::Call { func, args }) = program.statements.first().unwrap() {
            assert_eq!(format!("{}", *func), "add");
            assert_eq!(args.iter().map(|s| format!("{}", s)).collect::<Vec<String>>(), vec!["1", "2"]);
        } else {
            assert!(false)
        }
    }


    #[test]
    fn parser_errors() {
        let invalid_input = "let x 5;";
        let program = Parser::new(Lexer::new(invalid_input.to_owned())).parse();
        assert_eq!(program.errors.len(), 1);
    }

    #[test]
    fn integer_float_literal_expr() {
        let program = Parser::new(Lexer::new("5;6.1;".to_owned())).parse();
        let expected = vec![
            Statement::Expression(Expression::Literal(Literal::Int(5))),
            Statement::Expression(Expression::Literal(Literal::Float(6.1))),
        ];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn prefix_expr() {
        let program = Parser::new(Lexer::new("!5;-15; !false".to_owned())).parse();
        let expected = vec![
            Statement::Expression(Expression::Prefix(Prefix::Bang, Box::new(Expression::Literal(Literal::Int(5))))),
            Statement::Expression(Expression::Prefix(Prefix::Minus, Box::new(Expression::Literal(Literal::Int(15))))),
            Statement::Expression(Expression::Prefix(Prefix::Bang, Box::new(Expression::Literal(Literal::Bool(false))))),
        ];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn infix_expr() {
        let program = Parser::new(Lexer::new("5 + 15 * 18;".to_owned())).parse();
        assert_eq!(0, program.errors.len());

        let expected = vec![
            Statement::Expression(
                Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Infix(
                        Infix::Mul,
                        Box::new(Expression::Literal(Literal::Int(15))),
                        Box::new(Expression::Literal(Literal::Int(18))),
                    )),
                ),
            ),
        ];

        assert_eq!(program.statements, expected);
    }


    #[test]
    fn string_literal() {
        let program = Parser::new(Lexer::new(r#""hello world""#.to_owned())).parse();
        let expected = vec![
            Statement::Expression(Expression::Literal(Literal::String(String::from("hello world")))),
        ];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn operator_precedence() {
        let data = vec![
            ("-a * b", "((-a) * b)"),
            ("a ^ b % c * d", "(((a ^ b) % c) * d)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"),
        ];

        for &(input, expected) in data.iter() {
            let program = Parser::new(Lexer::new(input.to_owned())).parse();
            assert_eq!(program.errors.len(), 0);
            assert_eq!(format!("{}", program.statements.first().unwrap()), expected);
        }
    }

    #[test]
    fn list_literal() {
        let tests = vec![
            ("[1, 2 * 2, 3 + 3]", "[1, (2 * 2), (3 + 3)]"),
            ("[]", "[]"),
            ("my_list[1]", "(my_list[1])")
        ];

        for &(input, expected) in tests.iter() {
            let program = Parser::new(Lexer::new(input.to_string())).parse();
            assert_eq!(program.to_string(), expected)
        }
    }
}