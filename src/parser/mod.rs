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

use token::{Token, TokenKind};
use ast;

pub enum ParseError {
    UnexpectedToken {
        found: Token,
        expected: Vec<TokenKind>,
    },
}

use std::fmt;
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::UnexpectedToken { ref found, ref expected } => {
                try!(write!(f, "expected one of "));

                let mut sep = "";
                for tk in expected {
                    try!(write!(f, "\"{}\"{}", tk, sep));
                    sep = ", ";
                }

                write!(f, "found \"{}\"", found)
            }
        }
    }
}

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
        let next = self.tokens.pop().unwrap_or(Token {
            kind: TokenKind::Eof,
            value: None,
        });
        self.token = next;
    }

    /// Consume the next token, asserting its kind is equal to `expected`.
    fn eat(&mut self, expected: TokenKind) {
        assert_eq!(self.token.kind, expected);
        self.bump();
    }

    /// Parse a package clause (e.g. `package main`).
    fn parse_package_clause(&mut self) -> String {
        self.eat(TokenKind::Package);

        match self.token.kind {
            TokenKind::Ident => {
                // XXX: cloning.
                let v = self.token.value.clone();
                self.bump();
                v.unwrap()
            }
            _ => panic!("expected identifier"),
        }
    }

    /// Parse any number of import declarations.
    fn parse_import_decls(&mut self) -> Vec<ast::ImportDecl> {
        let mut decls = Vec::new();

        loop {
            match self.token.kind {
                TokenKind::Import => {
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

        self.eat(TokenKind::Import);
        let mut specs = Vec::new();

        match self.token.kind {
            // Long import declaration.
            TokenKind::LParen => {
                self.bump();

                // There may be multiple `ImportSpec`s in a single "long" import declaration.
                loop {
                    match self.token.kind {
                        // XXX: Should we _know_ that import specs always start with a string
                        // literal? I'm not sure.
                        TokenKind::RParen => {
                            break;
                        }
                        _ => {
                            specs.push(self.parse_import_spec());
                        }
                    }
                }
            }
            // Short import (single ImportSpec).
            _ => specs.push(self.parse_import_spec()),
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
        let kind = match self.token.kind {
            // Glob import.
            TokenKind::Dot => {
                self.bump();
                ast::ImportKind::Glob
            }
            TokenKind::Ident => {
                self.bump();
                ast::ImportKind::Alias(self.token.value.clone().unwrap())
            }
            _ => ast::ImportKind::Normal,
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

        match self.token.kind {
            // FunctionDecl
            TokenKind::Func => {
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

        self.eat(TokenKind::Func);
        let name = self.parse_ident();
        let signature = self.parse_func_signature();

        let body = match self.token.kind {
            // This function has a body, parse it.
            TokenKind::LBrace => self.parse_block(),
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

        let result = match self.token.kind {
            // An opening parenthesis! We can parse an output parameter list.
            TokenKind::LParen => self.parse_func_params(),
            // Brace = no return type, but a body. We don't care about the body in this function.
            // Semicolon = no return type and no body.
            TokenKind::LBrace | TokenKind::Semicolon => ast::Parameters::empty(),
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
        self.eat(TokenKind::LParen);

        let mut decls = Vec::new();

        // The parameter list is optional.
        match self.token.kind {
            TokenKind::Ident | TokenKind::Ellipsis => {
                decls.push(self.parse_parameter_decl());

                while let TokenKind::Comma = self.token.kind {
                    self.eat(TokenKind::Comma);
                    decls.push(self.parse_parameter_decl());
                }
            }
            _ => {}
        }
        self.eat(TokenKind::RParen);

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
        if let TokenKind::Ident = self.token.kind {
            // Grammar:
            // IdentifierList = identifier { "," identifier } .
            idents.push(self.parse_ident());

            while let TokenKind::Comma = self.token.kind {
                self.eat(TokenKind::Comma);
                idents.push(self.parse_ident());
            }
        }

        // So is the ellipsis that indicates a variadic func.
        if let TokenKind::Ellipsis = self.token.kind {
            self.eat(TokenKind::Ellipsis);
            variadic = true;
        }

        // The type is mandatory.
        let typ = self.parse_type();

        ast::ParameterDecl {
            identifiers: idents,
            typ: typ,
            variadic: variadic,
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

        match self.token.kind {
            // A Type may be surrounded in parentheses, in which case we simply eat the
            // parentheses and recurse.
            TokenKind::LParen => {
                self.eat(TokenKind::LParen);
                let typ = self.parse_type();
                self.eat(TokenKind::RParen);
                typ
            }
            // If a Type starts with an identifier, it can only be a TypeName.
            TokenKind::Ident => {
                let part1 = self.parse_ident();

                let name;
                let package;
                match self.token.kind {
                    // This is a qualified identifier.
                    // XXX: should we move that to a new function?
                    // Qualified idents can only appear in:
                    // - types (that's what we're parsing)
                    // - operands.
                    TokenKind::Colon => {
                        // XXX: I don't like this pattern.
                        self.eat(TokenKind::Colon);
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
        self.eat(TokenKind::LBrace);

        let mut statements = Vec::new();
        while self.token.kind.can_start_statement() {
            statements.push(self.parse_statement());
            self.eat(TokenKind::Semicolon);
        }

        self.eat(TokenKind::RBrace);
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

        match self.token.kind {
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
            t if t.can_start_expr() => self.parse_simple_stmt().into(),
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
        match self.token.kind {
            TokenKind::Ident => {
                self.bump();
                self.token.value.clone().unwrap()
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

        match self.token.kind {
            TokenKind::Str => {
                self.bump();
                // XXX TODO FIXME: we HAVE to interpret escape sequences!
                // For now, do nothing.
                self.token.value.clone().unwrap()
            }
            TokenKind::StrRaw => {
                self.bump();
                // Nothing to interpret, move along.
                self.token.value.clone().unwrap()
            }
            _ => panic!("unexpected token"),
        }
    }
}

pub fn parse_tokens(tokens: Vec<Token>) -> ast::SourceFile {
    let parser = Parser::new(tokens);
    parser.parse()
}
