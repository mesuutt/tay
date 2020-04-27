#[cfg(test)]
mod test {
    use crate::lexer::{Token, Lexer};
    use pretty_assertions::assert_eq;

    #[test]
    fn read_char() {
        let mut l = Lexer::new("()".to_owned());
        assert_eq!(l.position, 0);
        l.read_char();
        assert_eq!(l.position, 1);
        assert_eq!(l.read_position, 2);
        assert_eq!(l.ch, ')');
    }

    #[test]
    fn tokens() {
        let input = r#"let five = 5;
let ten = 10 ^ 2 % 4;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
my_float = 1.2;

< <= == != > >=
"foo"
"foo bar"
let a = "hello";
ident_with_num_1
[1, 2]
// comment 1
/* comment 2 */
"#;
        let expected = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Caret,
            Token::Int(String::from("2")),
            Token::Percent,
            Token::Int(String::from("4")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Ident(String::from("my_float")),
            Token::Assign,
            Token::Float(String::from("1.2")),
            Token::Semicolon,
            Token::Lt,
            Token::Lte,
            Token::Eq,
            Token::NotEq,
            Token::Gt,
            Token::Gte,
            Token::String(String::from("foo")),
            Token::String(String::from("foo bar")),
            Token::Let,
            Token::Ident(String::from("a")),
            Token::Assign,
            Token::String(String::from("hello")),
            Token::Semicolon,
            Token::Ident(String::from("ident_with_num_1")),
            Token::LBracket,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::RBracket,
            Token::EndOfFile,
        ];

        let mut lex = Lexer::new(input.to_owned());
        for i in expected {
            let t = lex.next_token();
            assert_eq!(t, i);
        }
    }

    #[test]
    fn end_of_file() {
        let input = "a+b";
        let expected = vec![
            Token::Ident(String::from("a")),
            Token::Plus,
            Token::Ident(String::from("b")),
            Token::EndOfFile
        ];

        let mut lex = Lexer::new(input.to_owned());
        for i in expected {
            let t = lex.next_token();
            assert_eq!(t, i);
        }
    }

    #[test]
    fn row_col() {
        let input = r#"
let a = foo;
let b = bar;
// my comment"#;
        let expected = vec![
            ('\n', 1, 1),
            (' ', 2, 4),
            (' ', 2, 6),
            (' ', 2, 8),
            (';', 2, 12),
            ('\n', 2, 13),
            (' ', 3, 4),
            (' ', 3, 6),
            (' ', 3, 8),
            (';', 3, 12),
            ('\n', 3, 13),
            ('\0', 4, 14),
        ];

        let mut lex = Lexer::new(input.to_owned());
        for i in expected {
            assert_eq!((lex.ch, lex.line, lex.col), i);
            // println!("{:?}", (lex.ch, lex.row, lex.col));
            lex.next_token();
        }
    }

    #[test]
    fn span_slice() {
        let input = r#"foo
// comment
let
"#;
        let expected = vec![
            ("foo", 0..2),
            ("let", 15..17)
        ];

        let mut lex = Lexer::new(input.to_string());
        for (slice, span) in expected {
            lex.next_token();
            assert_eq!(lex.span(), span);
            assert_eq!(lex.slice(), slice);
        }
    }

    #[test]
    fn line_slice() {
        let input = r#"123456789
// comment
"hello world!"
"#;
        let expected = vec![
            (0..15, "123456789"),
            (0..12, r#""hello world!""#)
        ];

        let mut lex = Lexer::new(input.to_string());
        for (_span, slice) in expected {
            let t = lex.next_token();
            assert_eq!(lex.line_slice(), slice);
        }
    }

}