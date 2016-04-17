//! # Parser
//!
//! Turn a list of tokens into an AST.
//!
//! ## To do
//!
//! - Use `Result`s for error handling, potentially with the question mark operator if we don't
//! mind using nightlies only.
//!
//! ## Unresolved questions
//!
//! - Is the pattern of inspecting the last element by reference, then popping it inside a function
//! and asserting it is equal to something, bad? It lets smaller functions not worry about what
//! came before them, **but** it also duplicates knowledge of what a construct may start with and
//! may hurt performance.
//!
//! - Should we pop/push from a vector, or use a slice and an index? Popping frees up memory as we
//! go, but is most likely slower.

use token::{self, Token, DelimToken, TokenKind, Literal, Keyword};
use ast;
use std::iter::Iterator;

#[cfg(test)]
mod test;

pub struct Parser {
    /// A list of tokens, **in reverse order**.
    /// This allows efficient push and pop operations (appending or popping from the beginning of a
    /// vector is horribly inefficient, AFAIK).
    tokens: Vec<Token>,
    /// The current token.
    token: Token,
}

impl Parser {
    /// Create a new `Parser` from a list of tokens.
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        // See doc comment on `tokens` field.
        tokens.reverse();
        Parser {
            token: tokens.pop().unwrap(),
            tokens: tokens,
        }
    }

    /// Parse the tokens into a SourceFile (AST).
    pub fn parse(mut self) -> ast::SourceFile {
        let package_name = self.parse_package_clause();
        let import_decls = self.parse_import_decls();
        let top_level_decls = self.parse_top_level_decls();

        ast::SourceFile {
            package: package_name,
            import_decls: import_decls,
            top_level_decls: top_level_decls,
        }
    }

    // === Utility functions ===

    /// Advance the parser by one token.
    fn bump(&mut self) {
        let next = self.tokens.pop().unwrap_or(Token::Eof);
        self.token = next;
    }

    /// Consume the next token, asserting it is equal to `expected`.
    fn eat(&mut self, expected: &Token) {
        assert_eq!(&self.token, expected);
        self.bump();
    }

    fn eat_keyword(&mut self, kw: Keyword) {
        self.eat(&Token::Keyword(kw));
    }

    /// Parse a package clause (e.g. `package main`).
    fn parse_package_clause(&mut self) -> String {
        self.eat_keyword(Keyword::Package);

        match self.token.clone() {
            Token::Ident(s) => {
                self.bump();
                s
            }
            _ => panic!("expected identifier"),
        }
    }

    /// Parse any number of import declarations.
    fn parse_import_decls(&mut self) -> Vec<ast::ImportDecl> {
        let mut decls = Vec::new();

        loop {
            match self.token {
                Token::Keyword(Keyword::Import) => {
                    decls.push(self.parse_import_decl());
                }
                _ => return decls,
            }
        }
    }

    /// Parse an import declaration, which is made up of one or more import specs.
    /// Simple example with a single spec: `import "fmt"`.
    fn parse_import_decl(&mut self) -> ast::ImportDecl {
        // Grammar:
        //
        // ```
        // ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
        // ```

        self.eat_keyword(Keyword::Import);
        let mut specs = Vec::new();

        match self.token.kind() {
            // Long import declaration.
            TokenKind::LParen => {
                self.bump();

                // There may be multiple `ImportSpec`s in a single "long" import declaration.
                loop {
                    match self.token {
                        // XXX: Should we _know_ that import specs always start with a string
                        // literal? I'm not sure.
                        Token::CloseDelim(DelimToken::Paren) => {
                            break;
                        }
                        _ => {
                            specs.push(self.parse_import_spec());
                        }
                    }
                }
            }
            // Short import (single ImportSpec).
            ref t => specs.push(self.parse_import_spec()),
        }

        ast::ImportDecl { specs: specs }
    }

    /// Parse an "import spec".
    fn parse_import_spec(&mut self) -> ast::ImportSpec {
        // Grammar:
        //
        // ```
        // ImportSpec       = [ "." | PackageName ] ImportPath .
        // ```

        // Does this package spec define an alias?
        let kind = match self.token.clone() {
            // Glob import.
            Token::Dot => {
                self.bump();
                ast::ImportKind::Glob
            }
            Token::Ident(alias) => {
                self.bump();
                ast::ImportKind::Alias(alias)
            }
            ref t => ast::ImportKind::Normal,
        };

        // The next token MUST be a string literal (interpreted or raw).
        let path = self.parse_string_lit();

        ast::ImportSpec {
            path: path,
            kind: kind,
        }
    }

    /// Parse any number of top-level declarations (see TopLevelDecl docs).
    // Grammar:
    //
    // TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
    fn parse_top_level_decls(&mut self) -> Vec<ast::TopLevelDecl> {
        let mut decls = Vec::new();

        match self.token {
            // FunctionDecl
            Token::Keyword(Keyword::Func) => {
                let fd = self.parse_func_decl();
                decls.push(ast::TopLevelDecl::Func(fd));
            }
            _ => panic!("unexpected token"),
        }

        decls
    }

    /// Parse a full function declaration (including signature, name, and block).
    fn parse_func_decl(&mut self) -> ast::FuncDecl {
        // Grammar:
        // FunctionDecl = "func" FunctionName ( Function | Signature ) .
        // FunctionName = identifier .
        // Function     = Signature FunctionBody .
        // FunctionBody = Block .

        self.eat_keyword(Keyword::Func);
        let name = self.parse_ident();
        let signature = self.parse_func_signature();

        let body = match self.token {
            // This function has a body, parse it.
            Token::OpenDelim(DelimToken::Brace) => self.parse_block(),
            // Empty body.
            _ => vec![],
        };

        ast::FuncDecl {
            name: name,
            signature: signature,
            body: body,
        }
    }

    /// Parse a function _signature_ - i.e., just the parameter and result types of a func.
    fn parse_func_signature(&mut self) -> ast::FuncSignature {
        // Grammar:
        //
        // Signature      = Parameters [ Result ] .
        // Result         = Parameters | Type .
        //
        // Example signature:
        //
        // (int, int, float64) (float64, *[]int)
        //
        // The parameters and the result of a function have a similar grammar,
        // however there's a small difference. The Go spec says:
        //
        // "Parameter and result lists are always parenthesized except that if there is exactly one
        // unnamed result it may be written as an unparenthesized type."
        let parameters = self.parse_func_params();

        let result = match self.token {
            // An opening parenthesis! We can parse an output parameter list.
            Token::OpenDelim(DelimToken::Paren) => self.parse_func_params(),
            // Brace = no return type, but a body. We don't care about the body in this function.
            // Semicolon = no return type and no body.
            Token::OpenDelim(DelimToken::Brace) | Token::Semicolon => ast::Parameters::empty(),
            // Otherwise, a single, unnamed return type.
            _ => ast::Parameters::from_single_type(self.parse_type()),
        };

        ast::FuncSignature {
            parameters: parameters,
            result: result,
        }
    }

    /// Parse function parameters, as defined by the Go grammar.
    ///
    /// This may be used to parse the return types of a function if they are prefixed with a
    /// parenthesis, and follow the same grammar as input parameters.
    /// Parameters may be named or unnamed.
    fn parse_func_params(&mut self) -> ast::Parameters {
        // Grammar:
        //
        // Parameters     = "(" [ ParameterList [ "," ] ] ")" .
        // ParameterList  = ParameterDecl { "," ParameterDecl } .
        // ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
        self.eat(&Token::OpenDelim(DelimToken::Paren));

        let mut decls = Vec::new();

        // The parameter list is optional.
        match self.token {
            Token::Ident(_) | Token::Ellipsis => {
                decls.push(self.parse_parameter_decl());

                while let Token::Comma = *self.tokens.last().expect("EOF") {
                    self.eat(&Token::Comma);
                    decls.push(self.parse_parameter_decl());
                }
            }
            _ => {}
        }
        self.eat(&Token::CloseDelim(DelimToken::Paren));

        // XXX: do we _need_ Parameters to be a type by itself?
        ast::Parameters { decls: decls }
    }

    /// Parse a "parameter decl".
    fn parse_parameter_decl(&mut self) -> ast::ParameterDecl {
        // Grammar:
        // ParameterDecl  = [ IdentifierList ] [ "..." ] Type .

        let mut idents = Vec::new();
        let mut variadic = false;

        // The identifier list is optional.
        if let Token::Ident(_) = self.token {
            // Grammar:
            // IdentifierList = identifier { "," identifier } .
            idents.push(self.parse_ident());

            while let Token::Comma = self.token {
                self.eat(&Token::Comma);
                idents.push(self.parse_ident());
            }
        }

        // So is the ellipsis that indicates a variadic func.
        if let Token::Ellipsis = self.token {
            self.eat(&Token::Ellipsis);
            variadic = true;
            // TODO: variadic funcs
            unimplemented!()
        }

        // The type is mandatory.
        let typ = self.parse_type();

        ast::ParameterDecl {
            identifiers: idents,
            typ: typ,
        }
    }

    /// Parse a single type (e.g. `[]string`).
    // XXX: type declarations can be very complex; this function needs attention.
    fn parse_type(&mut self) -> ast::Type {
        // Grammar:
        //
        // Type      = TypeName | TypeLit | "(" Type ")" .
        // TypeName  = identifier | QualifiedIdent .
        // TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
        // 	    SliceType | MapType | ChannelType .

        // TypeName = a (potentially qualified with a module path) identifier
        // TypeLit = more complex type, this is where it gets interesting

        match self.token {
            // A Type may be surrounded in parentheses, in which case we simply eat the
            // parentheses and recurse.
            Token::OpenDelim(DelimToken::Paren) => {
                self.eat(&Token::OpenDelim(DelimToken::Paren));
                let typ = self.parse_type();
                self.eat(&Token::CloseDelim(DelimToken::Paren));
                typ
            }
            // If a Type starts with an identifier, it can only be a TypeName.
            Token::Ident(_) => {
                let part1 = self.parse_ident();

                let name;
                let package;
                match self.token {
                    // This is a qualified identifier.
                    // XXX: should we move that to a new function?
                    // Qualified idents can only appear in:
                    // - types (that's what we're parsing)
                    // - operands.
                    Token::Colon => {
                        // XXX: I don't like this pattern.
                        self.eat(&Token::Colon);
                        let part2 = self.parse_ident();

                        package = Some(part1);
                        name = part2;
                    }
                    // Not qualified? Doesn't matter.
                    _ => {
                        name = part1;
                        package = None;
                    }
                }

                ast::Type::Plain(ast::MaybeQualifiedIdent {
                    package: package,
                    name: name,
                })
            }
            _ => unimplemented!(),
        }
    }

    fn parse_block(&mut self) -> Vec<ast::Statement> {
        // Grammar:
        // Block = "{" StatementList "}" .
        // StatementList = { Statement ";" } .
        self.eat(&Token::OpenDelim(DelimToken::Brace));

        let mut statements = Vec::new();
        while self.token.kind().can_start_statement() {
            statements.push(self.parse_statement());
            self.eat(&Token::Semicolon);
        }

        self.eat(&Token::CloseDelim(DelimToken::Brace));
        statements
    }

    // XXX: needs thorough review.
    fn parse_statement(&mut self) -> ast::Statement {
        // Statement =
        // 	Declaration | LabeledStmt | SimpleStmt |
        // 	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
        // 	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
        // 	DeferStmt .
        //
        // SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment |
        //  ShortVarDecl .
        use ast::Statement;

        match self.token.kind() {
            TokenKind::Type |
            TokenKind::Var |
            TokenKind::Const => self.parse_decl_stmt().into(),
            TokenKind::Go => self.parse_go_stmt().into(),
            TokenKind::Defer => self.parse_defer_stmt().into(),
            TokenKind::Return => self.parse_return_stmt().into(),
            TokenKind::If => self.parse_if_stmt().into(),
            TokenKind::Switch => self.parse_switch_stmt().into(),
            TokenKind::Select => self.parse_select_stmt().into(),
            TokenKind::For => self.parse_for_stmt().into(),
            // All simple statements start with something expression-like.
            ref t if t.can_start_expr() => self.parse_simple_stmt().into(),
            TokenKind::LBrace => ast::Block(self.parse_block()).into(),
            TokenKind::RBrace => {
                // a semicolon may be omitted before a closing "}"
                ast::EmptyStmt.into()
            }
            _ => panic!("unexpected token"),
        }
    }

    fn parse_go_stmt(&mut self) -> ast::GoStmt {
        unimplemented!()
    }
    fn parse_defer_stmt(&mut self) -> ast::DeferStmt {
        unimplemented!()
    }
    fn parse_return_stmt(&mut self) -> ast::ReturnStmt {
        unimplemented!()
    }
    fn parse_if_stmt(&mut self) -> ast::IfStmt {
        unimplemented!()
    }
    fn parse_switch_stmt(&mut self) -> ast::SwitchStmt {
        unimplemented!()
    }
    fn parse_select_stmt(&mut self) -> ast::SelectStmt {
        unimplemented!()
    }
    fn parse_for_stmt(&mut self) -> ast::ForStmt {
        unimplemented!()
    }
    fn parse_simple_stmt(&mut self) -> ast::SimpleStmt {
        unimplemented!()
    }
    fn parse_decl_stmt(&mut self) -> ast::SimpleStmt {
        unimplemented!()
    }

    /// Parse an identifier.
    // XXX: come back later. String interning and all that.
    fn parse_ident(&mut self) -> String {
        match self.token.clone() {
            Token::Ident(s) => {
                self.bump();
                s
            }
            _ => panic!("unexpected token"),
        }
    }

    /// Parse a string literal, whether interpreted or raw.
    /// This is useful because one will often expect a string literal without caring about its
    /// kind.
    fn parse_string_lit(&mut self) -> String {
        // Grammar:
        //
        // ```
        // string_lit             = raw_string_lit | interpreted_string_lit .
        // raw_string_lit         = "`" { unicode_char | newline } "`" .
        // interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
        // ```

        match self.token.clone() {
            Token::Literal(lit) => {
                self.bump();
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

pub fn parse(tokens: Vec<Token>) -> ast::SourceFile {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
