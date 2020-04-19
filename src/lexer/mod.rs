pub mod token;
#[cfg(test)]
mod test;

pub use token::Token;
use token::lookup_ident;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    input_len: usize,
    ch: char,
    pub row: usize,
    pub col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            input_len: input.len(),
            ch: '0',
            row: 0,
            col: 0,
        };
        lexer.read_char(); // initialize l.ch, l.position and l.read_position
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position == self.input_len {
            self.ch = '\0';
        } else if let Some(ch) = self.input.chars().nth(self.read_position) {
            self.ch = ch;
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    pub fn next_token(&mut self) -> Token {
        // if this fn called after already EOF, do not continue anymore.
        if self.position == self.input_len {
            return Token::EndOfFile;
        }

        let token: Token;

        self.skip_whitespace();

        match self.ch {
            '\0' => token = Token::EndOfFile,
            '-' => token = Token::Minus,
            '+' => token = Token::Plus,
            '*' => token = Token::Asterisk,
            '/' => if self.next_ch_is('/') {
                self.eat_line();
                return self.next_token();
            } else if self.next_ch_is('*') {
                self.eat_comment();
                return self.next_token();
            } else {
                token = Token::Slash
            }
            '%' => token = Token::Percent,
            '^' => token = Token::Caret,
            ',' => token = Token::Comma,
            ';' => token = Token::Semicolon,
            '(' => token = Token::LParen,
            ')' => token = Token::RParen,
            '{' => token = Token::LBrace,
            '}' => token = Token::RBrace,
            '=' => {
                if self.next_ch_is('=') {
                    self.read_char();
                    token = Token::Eq;
                } else {
                    token = Token::Assign
                }
            }
            '!' => {
                if self.next_ch_is('=') {
                    self.read_char();
                    token = Token::NotEq;
                } else {
                    token = Token::Bang
                }
            }
            '>' => {
                if self.next_ch_is('=') {
                    self.read_char();
                    token = Token::Gte;
                } else {
                    token = Token::Gt
                }
            }
            '<' => {
                if self.next_ch_is('=') {
                    self.read_char();
                    token = Token::Lte;
                } else {
                    token = Token::Lt
                }
            }
            '"' => {
                token = Token::String(self.read_string())
            }
            _ => {
                if self.ch.is_ascii_alphabetic() {
                    let literal = self.read_identifier();
                    return lookup_ident(literal);
                } else if self.ch.is_ascii_alphanumeric() {
                    let literal = self.read_number();
                    if literal.contains('.') {
                        return Token::Float(literal);
                    } else {
                        return Token::Int(literal);
                    }
                } else {
                    token = Token::Illegal(self.ch)
                }
            }
        }
        self.read_char();
        token
    }

    fn read_string(&mut self) -> String {
        self.read_char(); // Skip "
        let pos = self.position;
        loop {
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
            self.read_char();
        }
        self.input.chars().skip(pos).take(self.position - pos).collect::<String>()
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == '_' {
            self.read_char();
        }

        self.input.chars().skip(pos).take(self.position - pos).collect::<String>()
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphanumeric() || self.ch == '.' {
            self.read_char();
            /*if self.read_position > self.input_len {
                break;
            }*/
        }

        self.input.chars().skip(pos).take(self.position - pos).collect::<String>()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            if self.ch == '\n' || self.ch == '\r' {
                self.row += 1;
                self.col = 0;
            }
            self.read_char()
        }
    }

    fn eat_line(&mut self) {
        while self.ch != '\n' {
            self.read_char();
        }

        self.row += 1;
    }

    fn eat_comment(&mut self) {
        loop {
            if self.ch == '\n' || self.ch == '\r' {
                self.row += 1;
                self.col = 0;
            }
            self.read_char();

            if self.ch == '*' && self.next_ch_is('/') {
                self.read_char();
                self.read_char();
                break;
            }
        }
    }

    fn next_ch(&self) -> char {
        if let Some(ch) = self.input.chars().nth(self.read_position) {
            return ch;
        }

        '\0'
    }

    fn next_ch_is(&self, ch: char) -> bool {
        self.next_ch() == ch
    }
}
