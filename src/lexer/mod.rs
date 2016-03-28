use std::iter::Iterator;

mod token;
pub use self::token::*;

#[cfg(test)]
mod test;

pub struct Lexer<'src> {
    source: &'src str,
}

impl<'src> Lexer<'src> {
    pub fn new(s: &str) -> Lexer {
        Lexer { source: s }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        unimplemented!()
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    // Starting with an inline implementation, will break up into smaller pieces later.
    let mut lexer = Lexer::new(s);
    let tokens: Vec<Token> = lexer.collect();
    unimplemented!()
}
