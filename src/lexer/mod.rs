use std::iter::Iterator;

mod token;
pub use self::token::*;

#[cfg(test)]
mod test;

pub struct Lexer<'src> {
    source: &'src str,
    // XXX: is 'char' the right choice? Shouldn't we use u8? Not sure.
    /// The last character to be read.
    current_char: Option<char>,
}

impl<'src> Lexer<'src> {
    pub fn new(s: &str) -> Lexer {
        let mut l = Lexer {
            source: s,
            current_char: Some('\n'), // This is dummy temporary value which is never read.
        };

        l.bump();
        l
    }

    /// 'eat' one character.
    fn bump(&mut self) {
        unimplemented!()
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let c = self.current_char;

        match c.unwrap() {
            '(' => {
                self.bump();
                return Some(Token::OpenDelim(DelimToken::Paren));
            }
            ')' => {
                self.bump();
                return Some(Token::CloseDelim(DelimToken::Paren));
            }
            '{' => {
                self.bump();
                return Some(Token::OpenDelim(DelimToken::Brace));
            }
            '}' => {
                self.bump();
                return Some(Token::CloseDelim(DelimToken::Brace));
            }
            '[' => {
                self.bump();
                return Some(Token::OpenDelim(DelimToken::Bracket));
            }
            ']' => {
                self.bump();
                return Some(Token::CloseDelim(DelimToken::Bracket));
            }
            _ => panic!(),
        }

        unimplemented!()
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    // Starting with an inline implementation, will break up into smaller pieces later.
    let mut lexer = Lexer::new(s);
    let tokens: Vec<Token> = lexer.collect();
    unimplemented!()
}
