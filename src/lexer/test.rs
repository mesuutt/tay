#[cfg(test)]
mod test {
    use crate::lexer::{Token, Lexer};
    use pretty_assertions::assert_eq;

    #[test]
    fn read_char() {
        let mut l = Lexer::new("()");
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
// my comment
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
            Token::EndOfFile,
        ];

        let mut lex = Lexer::new(input);
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

        let mut lex = Lexer::new(input);
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
// my comment
"#;
        let expected = vec![
            ('\n', 0, 1),
            (' ', 1, 4),
            (' ', 1, 6),
            (' ', 1, 8),
            (';', 1, 12),
            ('\n', 1, 13),
            (' ', 2, 4),
            (' ', 2, 6),
            (' ', 2, 8),
            (';', 2, 12),
            ('\n', 2, 13),
            ('\0', 5, 2),
        ];

        let mut lex = Lexer::new(input);
        for i in expected {
            assert_eq!((lex.ch, lex.row, lex.col), i);
            // println!("{:?}", (lex.ch, lex.row, lex.col));
            lex.next_token();
        }
    }
}