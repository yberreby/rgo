//! # Parser
//!
//! Turn a list of tokens into an AST.

use std::mem;
use token::{Token, TokenAndOffset, TokenKind};
use ast;
use Position;

mod error;
pub use self::error::{PResult, Error, ErrorKind};

pub struct Parser<R: Iterator<Item = TokenAndOffset>> {
    /// Our source of tokens.
    /// Users can choose to read all the tokens up-front, or to read them lazily.
    reader: R,
    /// The current token.
    token: Token,
    /// Current byte offset from start of source string.
    offset: u32,
}

impl<R: Iterator<Item = TokenAndOffset>> Parser<R> {
    pub fn new(mut it: R) -> Parser<R> {
        let first_tok_and_pos = it.next().unwrap();
        Parser {
            token: first_tok_and_pos.token,
            offset: first_tok_and_pos.offset,
            reader: it,
        }
    }

    /// Parse the tokens into a SourceFile (AST).
    pub fn parse(mut self) -> PResult<ast::SourceFile> {
        let package_name = try!(self.parse_package_clause());
        let import_decls = try!(self.parse_import_decls());
        let top_level_decls = try!(self.parse_top_level_decls());

        Ok(ast::SourceFile {
            package: package_name,
            import_decls: import_decls,
            top_level_decls: top_level_decls,
        })
    }

    // === Utility functions ===

    /// Build a parse error.
    fn err(&self, kind: ErrorKind) -> Error {
        unimplemented!()
    }

    /// Advance the parser by one token.
    fn bump(&mut self) {
        let next = self.reader.next();
        let next_tok = next.map(|x| x.token).unwrap_or(Token {
            kind: TokenKind::Eof,
            value: None,
        });
        self.token = next_tok;
    }

    /// Advance the parser by one token and return the bumped token.
    pub fn bump_and_get(&mut self) -> Token {
        // The star is used a dummy token and replaced immediately.
        let old_token = mem::replace(&mut self.token,
                                     Token {
                                         kind: TokenKind::Star,
                                         value: None,
                                     });
        self.bump();
        old_token
    }

    /// Consume the next token, asserting its kind is equal to `expected`.
    fn eat(&mut self, expected: TokenKind) -> PResult<()> {
        if self.token.kind != expected {
            return Err(self.err(ErrorKind::unexpected_token(vec![expected], self.token.clone())));
        }
        self.bump();
        Ok(())
    }

    /// Parse a package clause (e.g. `package main`).
    fn parse_package_clause(&mut self) -> PResult<String> {
        try!(self.eat(TokenKind::Package));

        match self.token.kind {
            TokenKind::Ident => Ok(self.bump_and_get().value.unwrap()),
            _ => {
                Err(self.err(ErrorKind::unexpected_token(vec![TokenKind::Ident],
                                                         self.token.clone())))
            }
        }
    }

    /// Parse any number of import declarations.
    fn parse_import_decls(&mut self) -> PResult<Vec<ast::ImportDecl>> {
        let mut decls = Vec::new();

        loop {
            match self.token.kind {
                TokenKind::Import => {
                    decls.push(try!(self.parse_import_decl()));
                }
                _ => return Ok(decls),
            }
        }
    }

    /// Parse an import declaration, which is made up of one or more import specs.
    /// Simple example with a single spec: `import "fmt"`.
    fn parse_import_decl(&mut self) -> PResult<ast::ImportDecl> {
        // Grammar:
        //
        // ```
        // ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
        // ```

        try!(self.eat(TokenKind::Import));
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
                            specs.push(try!(self.parse_import_spec()));
                        }
                    }
                }
            }
            // Short import (single ImportSpec).
            _ => specs.push(try!(self.parse_import_spec())),
        }

        Ok(ast::ImportDecl { specs: specs })
    }

    /// Parse an "import spec".
    fn parse_import_spec(&mut self) -> PResult<ast::ImportSpec> {
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
            TokenKind::Ident => ast::ImportKind::Alias(self.bump_and_get().value.unwrap()),
            _ => ast::ImportKind::Normal,
        };

        // The next token MUST be a string literal (interpreted or raw).
        let path = try!(self.parse_string_lit());

        Ok(ast::ImportSpec {
            path: path,
            kind: kind,
        })
    }

    /// Parse any number of top-level declarations (see TopLevelDecl docs).
    // Grammar:
    //
    // TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
    fn parse_top_level_decls(&mut self) -> PResult<Vec<ast::TopLevelDecl>> {
        let mut decls = Vec::new();

        // FIXME: no loop + unfinished!

        match self.token.kind {
            // FunctionDecl
            TokenKind::Func => {
                let fd = try!(self.parse_func_decl());
                decls.push(ast::TopLevelDecl::Func(fd));
            }
            _ => {
                return Err(self.err(ErrorKind::unexpected_token(vec![TokenKind::Func],
                                                                self.token.clone())));
            }
        }

        Ok(decls)
    }

    /// Parse a full function declaration (including signature, name, and block).
    fn parse_func_decl(&mut self) -> PResult<ast::FuncDecl> {
        // Grammar:
        // FunctionDecl = "func" FunctionName ( Function | Signature ) .
        // FunctionName = identifier .
        // Function     = Signature FunctionBody .
        // FunctionBody = Block .

        try!(self.eat(TokenKind::Func));
        let name = try!(self.parse_ident());
        let signature = try!(self.parse_func_signature());

        let body = match self.token.kind {
            // This function has a body, parse it.
            TokenKind::LBrace => try!(self.parse_block()),
            // Empty body.
            _ => vec![],
        };

        Ok(ast::FuncDecl {
            name: name,
            signature: signature,
            body: body,
        })
    }

    /// Parse a function _signature_ - i.e., just the parameter and result types of a func.
    fn parse_func_signature(&mut self) -> PResult<ast::FuncSignature> {
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
        let parameters = try!(self.parse_func_params());

        let result = match self.token.kind {
            // An opening parenthesis! We can parse an output parameter list.
            TokenKind::LParen => try!(self.parse_func_params()),
            // Brace = no return type, but a body. We don't care about the body in this function.
            // Semicolon = no return type and no body.
            TokenKind::LBrace | TokenKind::Semicolon => ast::Parameters::empty(),
            // Otherwise, a single, unnamed return type.
            _ => ast::Parameters::from_single_type(try!(self.parse_type())),
        };

        Ok(ast::FuncSignature {
            parameters: parameters,
            result: result,
        })
    }

    /// Parse function parameters, as defined by the Go grammar.
    ///
    /// This may be used to parse the return types of a function if they are prefixed with a
    /// parenthesis, and follow the same grammar as input parameters.
    /// Parameters may be named or unnamed.
    fn parse_func_params(&mut self) -> PResult<ast::Parameters> {
        // Grammar:
        //
        // Parameters     = "(" [ ParameterList [ "," ] ] ")" .
        // ParameterList  = ParameterDecl { "," ParameterDecl } .
        // ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
        try!(self.eat(TokenKind::LParen));

        let mut decls = Vec::new();

        // The parameter list is optional.
        match self.token.kind {
            TokenKind::Ident | TokenKind::Ellipsis => {
                decls.push(try!(self.parse_parameter_decl()));

                while let TokenKind::Comma = self.token.kind {
                    self.eat(TokenKind::Comma);
                    decls.push(try!(self.parse_parameter_decl()));
                }
            }
            _ => {}
        }
        try!(self.eat(TokenKind::RParen));

        // XXX: do we _need_ Parameters to be a type by itself?
        Ok(ast::Parameters { decls: decls })
    }

    /// Parse a "parameter decl".
    fn parse_parameter_decl(&mut self) -> PResult<ast::ParameterDecl> {
        // Grammar:
        // ParameterDecl  = [ IdentifierList ] [ "..." ] Type .

        let mut idents = Vec::new();
        let mut variadic = false;

        // The identifier list is optional.
        if let TokenKind::Ident = self.token.kind {
            // Grammar:
            // IdentifierList = identifier { "," identifier } .
            idents.push(try!(self.parse_ident()));

            while let TokenKind::Comma = self.token.kind {
                try!(self.eat(TokenKind::Comma));
                idents.push(try!(self.parse_ident()));
            }
        }

        // So is the ellipsis that indicates a variadic func.
        if let TokenKind::Ellipsis = self.token.kind {
            try!(self.eat(TokenKind::Ellipsis));
            variadic = true;
        }

        // The type is mandatory.
        let typ = try!(self.parse_type());

        Ok(ast::ParameterDecl {
            identifiers: idents,
            typ: typ,
            variadic: variadic,
        })
    }

    /// Parse a single type (e.g. `[]string`).
    // XXX: type declarations can be very complex; this function needs attention.
    fn parse_type(&mut self) -> PResult<ast::Type> {
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
                try!(self.eat(TokenKind::LParen));
                let typ = try!(self.parse_type());
                try!(self.eat(TokenKind::RParen));
                Ok(typ)
            }
            // If a Type starts with an identifier, it can only be a TypeName.
            TokenKind::Ident => {
                let part1 = try!(self.parse_ident());

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
                        try!(self.eat(TokenKind::Colon));
                        let part2 = try!(self.parse_ident());

                        package = Some(part1);
                        name = part2;
                    }
                    // Not qualified? Doesn't matter.
                    _ => {
                        name = part1;
                        package = None;
                    }
                }

                Ok(ast::Type::Plain(ast::MaybeQualifiedIdent {
                    package: package,
                    name: name,
                }))
            }
            _ => unimplemented!(),
        }
    }

    fn parse_block(&mut self) -> PResult<Vec<ast::Statement>> {
        // Grammar:
        // Block = "{" StatementList "}" .
        // StatementList = { Statement ";" } .
        self.eat(TokenKind::LBrace);

        let mut statements = Vec::new();
        while self.token.kind.can_start_statement() {
            statements.push(try!(self.parse_statement()));
            try!(self.eat(TokenKind::Semicolon));
        }

        try!(self.eat(TokenKind::RBrace));
        Ok(statements)
    }

    // XXX: needs thorough review.
    fn parse_statement(&mut self) -> PResult<ast::Statement> {
        // Statement =
        // 	Declaration | LabeledStmt | SimpleStmt |
        // 	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
        // 	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
        // 	DeferStmt .
        //
        // SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment |
        //  ShortVarDecl .

        Ok(match self.token.kind {
            TokenKind::Type |
            TokenKind::Var |
            TokenKind::Const => try!(self.parse_decl_stmt()).into(),
            TokenKind::Go => try!(self.parse_go_stmt()).into(),
            TokenKind::Defer => try!(self.parse_defer_stmt()).into(),
            TokenKind::Return => try!(self.parse_return_stmt()).into(),
            TokenKind::If => try!(self.parse_if_stmt()).into(),
            TokenKind::Switch => try!(self.parse_switch_stmt()).into(),
            TokenKind::Select => try!(self.parse_select_stmt()).into(),
            TokenKind::For => try!(self.parse_for_stmt()).into(),
            // All simple statements start with something expression-like.
            t if t.can_start_expr() => try!(self.parse_simple_stmt()).into(),
            TokenKind::LBrace => ast::Block(try!(self.parse_block())).into(),
            TokenKind::RBrace => {
                // a semicolon may be omitted before a closing "}"
                ast::EmptyStmt.into()
            }
            _ => panic!("unexpected token"),
        })
    }

    fn parse_go_stmt(&mut self) -> PResult<ast::GoStmt> {
        unimplemented!()
    }
    fn parse_defer_stmt(&mut self) -> PResult<ast::DeferStmt> {
        unimplemented!()
    }
    fn parse_return_stmt(&mut self) -> PResult<ast::ReturnStmt> {
        unimplemented!()
    }
    fn parse_if_stmt(&mut self) -> PResult<ast::IfStmt> {
        unimplemented!()
    }
    fn parse_switch_stmt(&mut self) -> PResult<ast::SwitchStmt> {
        unimplemented!()
    }
    fn parse_select_stmt(&mut self) -> PResult<ast::SelectStmt> {
        unimplemented!()
    }
    fn parse_for_stmt(&mut self) -> PResult<ast::ForStmt> {
        unimplemented!()
    }
    fn parse_simple_stmt(&mut self) -> PResult<ast::SimpleStmt> {
        unimplemented!()
    }
    fn parse_decl_stmt(&mut self) -> PResult<ast::SimpleStmt> {
        unimplemented!()
    }

    /// Parse an identifier.
    // XXX: come back later. String interning and all that.
    fn parse_ident(&mut self) -> PResult<String> {
        match self.token.kind {
            TokenKind::Ident => {
                self.bump();
                Ok(self.token.value.clone().unwrap())
            }
            _ => {
                Err(self.err(ErrorKind::unexpected_token(vec![TokenKind::Ident],
                                                         self.token.clone())))
            }
        }
    }

    /// Parse a string literal, whether interpreted or raw.
    /// This is useful because one will often expect a string literal without caring about its
    /// kind.
    fn parse_string_lit(&mut self) -> PResult<String> {
        // Grammar:
        //
        // ```
        // string_lit             = raw_string_lit | interpreted_string_lit .
        // raw_string_lit         = "`" { unicode_char | newline } "`" .
        // interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
        // ```

        match self.token.kind {
            TokenKind::Str => {
                // XXX TODO FIXME: we HAVE to interpret escape sequences!
                // For now, do nothing.
                Ok(self.bump_and_get().value.unwrap())
            }
            TokenKind::StrRaw => {
                // Nothing to interpret, move along.
                Ok(self.bump_and_get().value.unwrap())
            }
            _ => {
                Err(self.err(ErrorKind::unexpected_token(vec![TokenKind::Str, TokenKind::StrRaw],
                                                         self.token.clone())))
            }
        }
    }
}

pub fn parse_tokens(tokens: Vec<TokenAndOffset>) -> ast::SourceFile {
    let parser = Parser::new(tokens.into_iter());
    parser.parse().unwrap()
}
