
#[cfg(test)]
pub mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast::{Statement, Ident, Literal, Expression, Prefix, Infix};

    #[test]
    fn let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;
        let z = 1.2;
        "#;

        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse();
        assert_eq!(prog.statements.len(), 3);
        assert_eq!(prog.errors.len(), 0);

        assert_eq!(prog.statements, vec![
            Statement::Let(Ident(String::from("x")), Expression::Literal(Literal::Int(5))),
            Statement::Let(Ident(String::from("y")), Expression::Literal(Literal::Int(10))),
            Statement::Let(Ident(String::from("z")), Expression::Literal(Literal::Float(1.2))),
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
        ];

        for &(input, expected) in data.iter() {
            let program = Parser::new(Lexer::new(input)).parse();
            assert_eq!(program.errors.len(), 0);
            assert_eq!(format!("{}", program.statements.first().unwrap()), expected);
        }
    }
}