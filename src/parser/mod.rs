//! # Parser
//!
//! Turn a list of tokens into an AST.
//!
//! ## Unresolved questions
//!
//! Is the pattern of inspecting the last element by reference, then popping it inside a
//! function and asserting it is equal to something bad?
//!
//! It lets smaller functions not worry about what came before them, **but** it also duplicates
//! knowledge of what a construct may start with and may hurt performance.

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
}

impl Parser {
    /// Create a new `Parser` from a list of tokens.
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        // See doc comment on `tokens` field.
        tokens.reverse();
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

    // === Utility functions ===

    /// Consume the next token, asserting it is equal to `expected`.
    fn eat(&mut self, expected: &Token) {
        assert_eq!(self.tokens.pop().as_ref(), Some(expected));
    }

    fn eat_keyword(&mut self, kw: Keyword) {
        assert_eq!(self.tokens.pop(), Some(Token::Keyword(kw)))
    }

    /// Parse a package clause (e.g. `package main`).
    fn parse_package_clause(&mut self) -> String {
        self.eat_keyword(Keyword::Package);

        match self.tokens.pop() {
            Some(Token::Ident(s)) => s,
            _ => panic!("expected identifier"),
        }
    }

    /// Parse any number of import declarations.
    fn parse_import_decls(&mut self) -> Vec<ImportDecl> {
        let mut decls = Vec::new();

        loop {
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

        self.eat_keyword(Keyword::Import);
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
        let path = self.parse_string_lit();

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
        let mut decls = Vec::new();

        match self.tokens.last().expect("EOF") {
            // FunctionDecl
            &Token::Keyword(Keyword::Func) => {
                let fd = self.parse_func_decl();
                decls.push(TopLevelDecl::Func(fd));
            }
            _ => panic!("unexpected token"),
        }

        decls
    }

    /// Parse a full function declaration (including signature, name, and block).
    // FunctionDecl = "func" FunctionName ( Function | Signature ) .
    // FunctionName = identifier .
    // Function     = Signature FunctionBody .
    // FunctionBody = Block .
    fn parse_func_decl(&mut self) -> ast::FuncDecl {
        self.eat_keyword(Keyword::Func);
        let name = self.parse_ident();
        let signature = self.parse_func_signature();

        let body = match self.tokens.last() {
            // This function has a body, parse it.
            Some(&Token::OpenDelim(DelimToken::Brace)) => self.parse_block(),
            // Empty body.
            _ => vec![],
        };

        ast::FuncDecl {
            name: name,
            signature: signature,
            body: body,
        }
    }

    /// Parse a function signature.
    fn parse_func_signature(&mut self) -> ast::FuncSignature {
        // Grammar:
        //
        // FunctionType   = "func" Signature .
        // Signature      = Parameters [ Result ] .
        // Result         = Parameters | Type .
        // Parameters     = "(" [ ParameterList [ "," ] ] ")" .
        // ParameterList  = ParameterDecl { "," ParameterDecl } .
        // ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
        //
        // Example signature:
        //
        // (int, int, float64) (float64, *[]int)

        // Strangely enough, the parameters and the result of a function have the same grammar,
        // so we do the same thing twice.
        let parameters = self.parse_func_params();
        let result = self.parse_func_params();

        ast::FuncSignature {
            parameters: parameters,
            result: result,
        }
    }


    fn parse_block(&mut self) -> Vec<ast::Statement> {
        unimplemented!()
    }

    /// Parse an identifier.
    // XXX: come back later. String interning and all that.
    fn parse_ident(&mut self) -> String {
        match self.tokens.pop().expect("EOF") {
            Token::Ident(s) => s,
            _ => panic!("unexpected token"),
        }
    }

    /// Parse a string literal, whether interpreted or raw.
    /// This is useful because one will often expect a string literal without caring about its
    /// kind.
    fn parse_string_lit(&mut self) -> String {
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
