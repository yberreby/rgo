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
        let package_name = self.parse_package_clause();
        let import_decls = self.parse_import_decls();
        let top_level_decls = self.parse_top_level_decls();

        SourceFile {
            package: package_name,
            import_decls: import_decls,
            top_level_decls: top_level_decls,
        }
    }

    fn bump(&mut self) -> Token {
        unimplemented!()
    }

    fn skip_ws(&mut self) {
        unimplemented!()
    }

    /// Consume the next token, asserting it is equal to `expected`.
    fn expect(&mut self, expected: Token) {
        unimplemented!()
    }

    fn parse_package_clause(&mut self) -> String {
        // Whitespace at the top of the file is irrelevant.
        self.skip_ws();
        self.expect(Token::Keyword(Keyword::Package));

        let t = self.bump();
        match t {
            Token::Ident(s) => s,
            _ => panic!("expected identifier"),
        }
    }

    fn parse_import_decls(&mut self) -> Vec<ImportDecl> {
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
