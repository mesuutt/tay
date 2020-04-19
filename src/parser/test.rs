#[cfg(test)]
pub mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast::{Statement, Ident, Literal, Expression, Prefix, Infix, BlockStatement};
    use pretty_assertions::assert_eq;

    #[test]
    fn let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;
        let z = 1.2;
        let foo = "hello";
        "#;

        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 4);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Let(Ident(String::from("x")), Expression::Literal(Literal::Int(5))),
            Statement::Let(Ident(String::from("y")), Expression::Literal(Literal::Int(10))),
            Statement::Let(Ident(String::from("z")), Expression::Literal(Literal::Float(1.2))),
            Statement::Let(Ident(String::from("foo")), Expression::Literal(Literal::String("hello".to_owned()))),
        ]);
    }


    #[test]
    fn return_statement() {
        let input = r#"
        return 1;
        return foo;
        "#;

        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 2);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Return(Expression::Literal(Literal::Int(1))),
            Statement::Return(Expression::Ident(Ident("foo".to_string()))),
        ]);
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) {x} else {y}";
        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 1);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Expression(Expression::If {
                condition: Box::new(
                    Expression::Infix(
                        Infix::Lt,
                        Box::new(Expression::Ident(Ident("x".to_string()))),
                        Box::new(Expression::Ident(Ident("y".to_string()))),
                    )),
                consequence: vec![Statement::Expression(Expression::Ident(Ident("x".to_string())))],
                alternative: Some(vec![Statement::Expression(Expression::Ident(Ident("y".to_string())))]),
            }),
        ]);
    }


    #[test]
    fn function() {
        let expected = vec![
            ("fn () {}", Vec::<&str>::new(), Vec::<&str>::new()),
            ("fn (x, y) {x * y}", vec!["x", "y"], vec!["(x * y)"]),
            ("fn (x, y, z) {x * y * z}", vec!["x", "y", "z"], vec!["((x * y) * z)"]),
        ];

        for (input, expect_param, expect_body) in expected {
            let mut p = Parser::new(Lexer::new(input));
            let program = p.parse();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.errors.len(), 0);

            if let Statement::Expression(Expression::Func { params, body }) = program.statements.first().unwrap() {
                assert_eq!(params.iter().map(|s| format!("{}", s)).collect::<Vec<String>>(), expect_param);
                assert_eq!(body.iter().map(|s| format!("{}", s)).collect::<Vec<String>>(), expect_body)
            } else {
                assert!(false)
            }
        }
    }


    #[test]
    fn call_expression() {
        let program = Parser::new(Lexer::new("add(1, 2)")).parse();
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
        let program = Parser::new(Lexer::new(invalid_input)).parse();
        assert_eq!(program.errors.len(), 1);
    }

    #[test]
    fn integer_float_literal_expr() {
        let program = Parser::new(Lexer::new("5;6.1;")).parse();
        let expected = vec![
            Statement::Expression(Expression::Literal(Literal::Int(5))),
            Statement::Expression(Expression::Literal(Literal::Float(6.1))),
        ];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn prefix_expr() {
        let program = Parser::new(Lexer::new("!5;-15; !false")).parse();
        let expected = vec![
            Statement::Expression(Expression::Prefix(Prefix::Bang, Box::new(Expression::Literal(Literal::Int(5))))),
            Statement::Expression(Expression::Prefix(Prefix::Minus, Box::new(Expression::Literal(Literal::Int(15))))),
            Statement::Expression(Expression::Prefix(Prefix::Bang, Box::new(Expression::Literal(Literal::Bool(false))))),
        ];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn infix_expr() {
        let program = Parser::new(Lexer::new("5 + 15 * 18;")).parse();
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
        let program = Parser::new(Lexer::new(r#""hello world""#)).parse();
        let expected = vec![
            Statement::Expression( Expression::Literal(Literal::String(String::from("hello world")))),
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
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")
        ];

        for &(input, expected) in data.iter() {
            let program = Parser::new(Lexer::new(input)).parse();
            assert_eq!(program.errors.len(), 0);
            assert_eq!(format!("{}", program.statements.first().unwrap()), expected);
        }
    }
}