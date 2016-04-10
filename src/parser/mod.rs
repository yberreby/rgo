use lexer::{Token, Keyword, DelimToken, Literal};
use ast::*;
use std::iter::Iterator;

#[cfg(test)]
mod test;

pub struct Parser {
    /// A list of tokens, **in reverse order**.
    /// This allows efficient push and pop operations (appending or popping from the beginning of a
    /// vector is horribly inefficient, AFAIK).
    tokens: Vec<Token>,
    pos: usize,
}

// unresolved question: whitespace and semicolon insertion.

impl Parser {
    /// Create a new `Parser` from a list of tokens.
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        // See doc comment on `tokens` field.
        tokens.reverse();
        Parser {
            tokens: tokens,
            pos: 0,
        }
    }

    /// Parse the tokens into a SourceFile (AST).
    pub fn parse(mut self) -> SourceFile {
        let package_name = self.parse_package_clause();
        let import_decls = self.parse_import_decls();
        let top_level_decls = self.parse_top_level_decls();

        SourceFile {
            package: package_name,
            import_decls: import_decls,
            top_level_decls: top_level_decls,
        }
    }

    // /// Peek at the next token.
    // fn current(&self) -> Option<&Token> {
    //     self.tokens.get(self.pos)
    // }

    // /// Peek at the next token.
    // fn next_token(&self) -> Option<&Token> {
    //     self.tokens.get(self.pos + 1)
    // }

    // /// Move the parser one token forward, returning the token that was consumed.
    // fn bump(&mut self) -> Option<Token> {
    //     self.pos += 1;
    //     self.tokens.pop()
    // }

    // fn skip_ws(&mut self) {
    //     while let Some(&Token::Whitespace) = self.current() {
    //         self.bump();
    //     }
    // }

    // /// Consume the next token, asserting it is equal to `expected`.
    // fn expect(&mut self, expected: &Token) {
    //     assert_eq!(self.current(), Some(expected));
    //     self.bump();
    // }

    /// For when whitespace is unimportant.
    fn skip_whitespace(&mut self) {
        // XXX: we may not need a while loop, since blocks of contiguous whitespace
        // are treated as a single token.
        while let Some(&Token::Whitespace) = self.tokens.last() {
            self.tokens.pop();
        }
    }

    /// Parse a package clause (e.g. `package main`).
    fn parse_package_clause(&mut self) -> String {
        // Whitespace at the top of the file is irrelevant.
        self.skip_whitespace();
        assert_eq!(self.tokens.pop(), Some(Token::Keyword(Keyword::Package)));

        match self.tokens.pop() {
            Some(Token::Ident(s)) => s,
            _ => panic!("expected identifier"),
        }
    }

    fn parse_import_decls(&mut self) -> Vec<ImportDecl> {
        // self.skip_whitespace();

        // match self.tokens.last() {
        //
        // }

        unimplemented!()
    }


    fn parse_top_level_decls(&mut self) -> Vec<TopLevelDecl> {
        unimplemented!()
    }
}


pub fn parse(tokens: Vec<Token>) -> SourceFile {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
