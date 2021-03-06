pub mod token;
#[cfg(test)]
mod test;

pub use token::Token;
use token::lookup_ident;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '0',
            line: 0,
            col: 0,
        };
        lexer.read_char(); // initialize l.ch, l.position and l.read_position
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position == self.input.len() {
            self.ch = '\0';
        } else if let Some(ch) = self.input.chars().skip(self.read_position).nth(0) {
            self.ch = ch;
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    pub fn next_token(&mut self) -> Token {
        // if this fn called after already EOF, do not continue anymore.
        if self.position == self.input.len() {
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
                self.skip_line();
                return self.next_token();
            } else if self.next_ch_is('*') {
                self.skip_multi_line_comment();
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
            '[' => token = Token::LBracket,
            ']' => token = Token::RBracket,
            ':' => token = Token::Colon,
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
                // identifiers must start with alphabet chars
                if self.ch.is_ascii_alphabetic() {
                    let literal = self.read_identifier();
                    return lookup_ident(literal);
                } else if self.ch.is_ascii_digit() || self.ch == '.' {
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
        // self.input.chars().skip(pos).take(self.position - pos).collect::<String>();
        self.input[pos..self.position].to_string()
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_alphanumeric() || self.ch == '_' {
            self.read_char();
        }

        // self.input.chars().skip(pos).take(self.position - pos).collect::<String>()
        self.input[pos..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_digit() || self.ch == '.' {
            self.read_char();
        }

        // self.input.chars().skip(pos).take(self.position - pos).collect::<String>()
        self.input[pos..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            if self.ch == '\n' || self.ch == '\r' {
                self.line += 1;
                self.col = 0;
            }
            self.read_char()
        }
    }

    fn skip_line(&mut self) {
        while self.ch != '\n' {
            self.read_char();
        }

        self.line += 1;
    }

    fn skip_multi_line_comment(&mut self) {
        loop {
            if self.ch == '\n' || self.ch == '\r' {
                self.line += 1;
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

    fn peek_char(&self) -> char {
        if let Some(ch) = self.input.chars().skip(self.read_position).nth(0) {
            return ch;
        }

        '\0'
    }

    fn next_ch_is(&self, ch: char) -> bool {
        self.peek_char() == ch
    }
}
