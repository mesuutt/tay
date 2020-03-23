
#[cfg(test)]
mod test {
    use crate::token::Token;
    #[test]
    fn tokens() {
        let input = "=+(){},;";
        let expected = vec![
            Token::LBrace,
        ];
    }
}