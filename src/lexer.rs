use crate::token::{
    Token, lookup_ident, IntegerSize,
};

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '0',
        };
        lexer.read_char(); // initialize l.ch, l.position and l.read_position
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '0';
        } else if let Some(ch) = self.input.chars().nth(self.read_position) {
            self.ch = ch;
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let token: Token;
        self.skip_whitespace();

        match self.ch {
            '=' => token = Token::Assign,
            ';' => token = Token::Semicolon,
            '(' => token = Token::LParen,
            ')' => token = Token::RParen,
            ',' => token = Token::Comma,
            '+' => token = Token::Plus,
            '{' => token = Token::LBrace,
            '}' => token = Token::RBrace,
            '0' => token = Token::EndOfFile,
            _ => {
                if self.ch.is_ascii_alphabetic() {
                    let literal = self.read_identifier();
                    return lookup_ident(literal);
                } else {
                    if self.ch.is_ascii_alphanumeric() {
                        let literal = self.read_number();
                        match literal.parse::<IntegerSize>() {
                            Ok(i) => return Token::Int(i),
                            Err(_) => panic!("integer literal parse error")
                        }
                    } else {
                        token = Token::Illegal(self.ch)
                    }
                }
            }
        }
        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphabetic() {
            self.read_char();
        }

        self.input.chars().skip(pos).take(self.position - pos).collect::<String>()
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphanumeric() {
            self.read_char();
        }

        self.input.chars().skip(pos).take(self.position - pos).collect::<String>()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::Token;
    use super::Lexer;

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
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);";
        let expected = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
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
        ];

        let mut lex = Lexer::new(input);
        for i in expected {
            let t = lex.next_token();
            assert_eq!(t, i);
        }
    }
}