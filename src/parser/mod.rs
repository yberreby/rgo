use lexer::{Token, Keyword, DelimToken, Literal};
use ast::*;
use std::iter::Iterator;

#[cfg(test)]
mod test;

pub struct Parser {
    tokens: Vec<Token>,
}

// unresolved question: whitespace and semicolon insertion.

impl Parser {
    /// Create a new `Parser` from a list of tokens.
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens: tokens }
    }

    /// Parse the tokens into a SourceFile (AST).
    pub fn parse(mut self) -> SourceFile {
        // Whitespace at the top of the file is irrelevant.
        self.skip_ws();
        self.expect(Token::Keyword(Keyword::Package));

        unimplemented!()
    }

    fn skip_ws(&mut self) {
        unimplemented!()
    }

    /// Consume the next token, asserting it is equal to `expected`.
    fn expect(&mut self, expected: Token) {
        unimplemented!()
    }
}


pub fn parse(tokens: Vec<Token>) -> SourceFile {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
