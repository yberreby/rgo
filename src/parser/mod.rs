use lexer::{Token, Keyword, DelimToken, Literal};
use ast;
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
        let mut decls = Vec::new();

        loop {
            self.skip_whitespace();
            match self.tokens.last() {
                Some(&Token::Keyword(Keyword::Import)) => {
                    decls.push(self.parse_import_decl());
                }
                _ => return decls,
            }
        }
    }

    /// Parse an import declaration made up of one or more import specs.
    /// Simple example with a single spec: `import "fmt"`.
    fn parse_import_decl(&mut self) -> ImportDecl {
        // Grammar:
        //
        // ```
        // ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
        // ```

        assert_eq!(self.tokens.pop(), Some(Token::Keyword(Keyword::Import)));
        let mut specs = Vec::new();

        match self.tokens.pop() {
            // Long import declaration.
            Some(Token::OpenDelim(DelimToken::Paren)) => {
                // There may be multiple `ImportSpec`s in a single "long" import declaration.
                loop {
                    match self.tokens.last() {
                        // XXX: Should we _know_ that import specs always start with a string
                        // literal? I'm not sure.
                        Some(&Token::CloseDelim(DelimToken::Paren)) => {
                            break;
                        }
                        Some(_) => {
                            specs.push(self.parse_import_spec());
                        }
                        _ => panic!("unexpected end of input"),
                    }
                }
            }
            // Short import (single ImportSpec).
            Some(t) => {
                // XXX: Put it back...
                self.tokens.push(t);
                specs.push(self.parse_import_spec());
            }
            _ => panic!("unexpected end of input"),
        }

        ImportDecl { specs: specs }
    }

    /// Parse an "import spec".
    fn parse_import_spec(&mut self) -> ImportSpec {
        // Grammar:
        //
        // ```
        // ImportSpec       = [ "." | PackageName ] ImportPath .
        // ```

        let path: String;

        // Does this package spec define an alias?
        let kind = match self.tokens.pop().expect("unexpected end of input") {
            // Glob import.
            Token::Dot => ImportKind::Glob,
            Token::Ident(alias) => ImportKind::Alias(alias),
            t => {
                // Let's put this token back.
                self.tokens.push(t);
                ImportKind::Normal
            }
        };

        // The next token MUST be a string literal (interpreted or raw).
        let path = self.parse_string();

        ImportSpec {
            path: path,
            kind: kind,
        }
    }

    /// Parse a top-level declaration (see TopLevelDecl docs).
    // Grammar:
    //
    // TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
    fn parse_top_level_decls(&mut self) -> Vec<TopLevelDecl> {
        unimplemented!()
    }

    /// Parse a string literal, whether interpreted or raw.
    /// This is useful because one will often expect a string literal without caring about its
    /// kind.
    fn parse_string(&mut self) -> String {
        // Syntax:
        //
        // ```
        // string_lit             = raw_string_lit | interpreted_string_lit .
        // raw_string_lit         = "`" { unicode_char | newline } "`" .
        // interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
        // ```

        match self.tokens.pop().expect("unexpected end of input") {
            Token::Literal(lit) => {
                match lit {
                    // Nothing to interpret, move along.
                    Literal::StrRaw(s) => s,
                    Literal::Str(s) => {
                        // XXX TODO FIXME: we HAVE to interpret escape sequences!
                        // For now, do nothing.
                        s
                    }
                    _ => panic!("unexpected literal token"),
                }
            }
            _ => panic!("unexpected token"),
        }
    }
}


pub fn parse(tokens: Vec<Token>) -> SourceFile {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
