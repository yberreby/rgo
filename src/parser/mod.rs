use std::mem;
use std::iter::Peekable;
use std::str::CharIndices;
use token::*;
use ast;

#[cfg(test)]
mod test;

mod error;
pub use self::error::{PResult, Error, ErrorKind};

pub struct Parser<R: Iterator<Item = TokenAndSpan>> {
    /// Our source of tokens.
    /// Users can choose to read all the tokens up-front, or to read them lazily.
    reader: Peekable<R>,
    /// The current token.
    token: Token,
    /// Current byte offset from start of source string.
    offset: u32,
}

impl<R: Iterator<Item = TokenAndSpan>> Parser<R> {
    pub fn new(mut it: R) -> Parser<R> {
        // TODO: handle missing tok gracefully.
        let first_tok_and_pos = it.next().expect("missing first token");
        debug!("first_tok_and_pos: {:?}", first_tok_and_pos);
        Parser {
            token: first_tok_and_pos.token,
            offset: first_tok_and_pos.span.start,
            reader: it.peekable(),
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
        Error {
            offset: self.offset,
            kind: kind,
        }
    }

    /// Advance the parser by one token.
    fn bump(&mut self) {
        trace!("bump");
        let next = self.reader.next();

        if let Some(ref tap) = next {
            self.offset = tap.span.start;
        }

        let next_tok = next.map(|x| x.token).unwrap_or(Token {
            kind: TokenKind::Eof,
            value: None,
        });
        self.token = next_tok;
    }

    /// Advance the parser by one token and return the bumped token.
    fn bump_and_get(&mut self) -> Token {
        // The star is used a dummy token and replaced immediately.
        let old_token = mem::replace(&mut self.token,
                                     Token {
                                         kind: TokenKind::Operator(Operator::Star),
                                         value: None,
                                     });
        self.bump();
        old_token
    }

    /// Peek at the kind of the next token.
    fn next_kind(&mut self) -> TokenKind {
        self.reader.peek().map(|x| x.token.kind).unwrap_or(TokenKind::Eof)
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
        trace!("parse_package_clause");

        try!(self.eat(TokenKind::Keyword(Keyword::Package)));

        let package_name = try!(self.parse_ident());
        try!(self.eat(TokenKind::Semicolon));
        Ok(package_name)
    }

    /// Parse any number of import declarations.
    fn parse_import_decls(&mut self) -> PResult<Vec<ast::ImportDecl>> {
        trace!("parse_import_decls");
        let mut decls = Vec::new();

        loop {
            match self.token.kind {
                TokenKind::Keyword(Keyword::Import) => {
                    decls.push(try!(self.parse_import_decl()));
                }
                _ => return Ok(decls),
            }
        }
    }

    /// Parse an import declaration, which is made up of one or more import specs.
    /// Simple example with a single spec: `import "fmt"`.
    fn parse_import_decl(&mut self) -> PResult<ast::ImportDecl> {
        trace!("parse_import_decl");
        // Grammar:
        //
        // ```
        // ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
        // ```

        try!(self.eat(TokenKind::Keyword(Keyword::Import)));
        let mut specs = Vec::new();

        match self.token.kind {
            // Long import declaration.
            TokenKind::Delim(Delim::LParen) => {
                self.bump();

                // There may be multiple `ImportSpec`s in a single "long" import declaration.
                loop {
                    match self.token.kind {
                        // XXX: Should we _know_ that import specs always start with a string
                        // literal? I'm not sure.
                        TokenKind::Delim(Delim::RParen) => {
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
        try!(self.eat(TokenKind::Semicolon));

        Ok(ast::ImportDecl { specs: specs })
    }

    /// Parse an "import spec".
    fn parse_import_spec(&mut self) -> PResult<ast::ImportSpec> {
        trace!("parse_import_spec");
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
        trace!("parse_top_level_decls");
        let mut decls = Vec::new();

        // FIXME: no loop + unfinished!

        match self.token.kind {
            // FunctionDecl
            TokenKind::Keyword(Keyword::Func) => {
                let fd = try!(self.parse_func_decl());
                decls.push(ast::TopLevelDecl::Func(fd));
            }
            _ => {
                let e = ErrorKind::unexpected_token(vec![TokenKind::Keyword(Keyword::Func)],
                                                    self.token.clone());
                return Err(self.err(e));
            }
        }

        Ok(decls)
    }

    /// Parse a full function declaration (including signature, name, and block).
    fn parse_func_decl(&mut self) -> PResult<ast::FuncDecl> {
        trace!("parse_func_decl");
        // Grammar:
        // FunctionDecl = "func" FunctionName ( Function | Signature ) .
        // FunctionName = identifier .
        // Function     = Signature FunctionBody .
        // FunctionBody = Block .

        try!(self.eat(TokenKind::Keyword(Keyword::Func)));
        let name = try!(self.parse_ident());
        let signature = try!(self.parse_func_signature());

        let body = match self.token.kind {
            // This function has a body, parse it.
            TokenKind::Delim(Delim::LBrace) => try!(self.parse_block()),
            // Empty body.
            _ => vec![],
        };
        try!(self.eat(TokenKind::Semicolon));

        Ok(ast::FuncDecl {
            name: name,
            signature: signature,
            body: body,
        })
    }

    /// Parse a function _signature_ - i.e., just the parameter and result types of a func.
    fn parse_func_signature(&mut self) -> PResult<ast::FuncSignature> {
        trace!("parse_func_signature");
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
            TokenKind::Delim(Delim::LParen) => try!(self.parse_func_params()),
            // Brace = no return type, but a body. We don't care about the body in this function.
            // Semicolon = no return type and no body.
            TokenKind::Delim(Delim::LBrace) | TokenKind::Semicolon => ast::Parameters::empty(),
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
        trace!("parse_func_params");
        // Grammar:
        //
        // Parameters     = "(" [ ParameterList [ "," ] ] ")" .
        // ParameterList  = ParameterDecl { "," ParameterDecl } .
        // ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
        try!(self.eat(TokenKind::Delim(Delim::LParen)));

        let mut decls = Vec::new();

        // The parameter list is optional.
        match self.token.kind {
            TokenKind::Ident | TokenKind::Ellipsis => {
                decls.push(try!(self.parse_parameter_decl()));

                while let TokenKind::Comma = self.token.kind {
                    try!(self.eat(TokenKind::Comma));
                    decls.push(try!(self.parse_parameter_decl()));
                }
            }
            _ => {}
        }
        try!(self.eat(TokenKind::Delim(Delim::RParen)));

        // XXX: do we _need_ Parameters to be a type by itself?
        Ok(ast::Parameters { decls: decls })
    }

    /// Parse a "parameter decl".
    fn parse_parameter_decl(&mut self) -> PResult<ast::ParameterDecl> {
        trace!("parse_parameter_decl");
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
        trace!("parse_type");
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
            TokenKind::Delim(Delim::LParen) => {
                try!(self.eat(TokenKind::Delim(Delim::LParen)));
                let typ = try!(self.parse_type());
                try!(self.eat(TokenKind::Delim(Delim::RParen)));
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
        trace!("parse_block");
        // Grammar:
        // Block = "{" StatementList "}" .
        // StatementList = { Statement ";" } .
        try!(self.eat(TokenKind::Delim(Delim::LBrace)));

        let mut statements = Vec::new();
        while self.token.kind.can_start_statement() {
            statements.push(try!(self.parse_statement()));
            try!(self.eat(TokenKind::Semicolon));
        }

        try!(self.eat(TokenKind::Delim(Delim::RBrace)));
        Ok(statements)
    }

    // XXX: needs thorough review.
    fn parse_statement(&mut self) -> PResult<ast::Statement> {
        trace!("parse_statement");
        // Statement =
        // 	Declaration | LabeledStmt | SimpleStmt |
        // 	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
        // 	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
        // 	DeferStmt .
        //
        // SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment |
        //  ShortVarDecl .

        Ok(match self.token.kind {
            TokenKind::Keyword(key) => {
                match key {
                    Keyword::Type |
                    Keyword::Var |
                    Keyword::Const => try!(self.parse_decl_stmt()).into(),
                    Keyword::Go => try!(self.parse_go_stmt()).into(),
                    Keyword::Defer => try!(self.parse_defer_stmt()).into(),
                    Keyword::Return => try!(self.parse_return_stmt()).into(),
                    Keyword::If => try!(self.parse_if_stmt()).into(),
                    Keyword::Switch => try!(self.parse_switch_stmt()).into(),
                    Keyword::Select => try!(self.parse_select_stmt()).into(),
                    Keyword::For => try!(self.parse_for_stmt()).into(),
                    _ => panic!("unexpected token"),
                }
            }
            // All simple statements start with something expression-like.
            t if t.can_start_expr() => try!(self.parse_simple_stmt()).into(),
            TokenKind::Delim(Delim::LBrace) => ast::Block(try!(self.parse_block())).into(),
            TokenKind::Delim(Delim::RBrace) => {
                // a semicolon may be omitted before a closing "}"
                ast::EmptyStmt.into()
            }
            _ => panic!("unexpected token"),
        })
    }

    fn parse_go_stmt(&mut self) -> PResult<ast::GoStmt> {
        trace!("parse_go_stmt");

        try!(self.eat(TokenKind::Keyword(Keyword::Go)));
        Ok(ast::GoStmt { call: try!(self.parse_expr()) })
    }

    fn parse_defer_stmt(&mut self) -> PResult<ast::DeferStmt> {
        trace!("parse_defer_stmt");

        try!(self.eat(TokenKind::Keyword(Keyword::Defer)));
        Ok(ast::DeferStmt { call: try!(self.parse_expr()) })
    }

    fn parse_return_stmt(&mut self) -> PResult<ast::ReturnStmt> {
        trace!("parse_return_stmt");
        try!(self.eat(TokenKind::Keyword(Keyword::Return)));
        Ok(ast::ReturnStmt { expr: try!(self.parse_expr()) })
    }
    fn parse_if_stmt(&mut self) -> PResult<ast::IfStmt> {
        trace!("parse_if_stmt");
        unimplemented!()
    }
    fn parse_switch_stmt(&mut self) -> PResult<ast::SwitchStmt> {
        trace!("parse_switch_stmt");
        unimplemented!()
    }
    fn parse_select_stmt(&mut self) -> PResult<ast::SelectStmt> {
        trace!("parse_select_stmt");
        unimplemented!()
    }
    fn parse_for_stmt(&mut self) -> PResult<ast::ForStmt> {
        trace!("parse_for_stmt");
        unimplemented!()
    }
    fn parse_simple_stmt(&mut self) -> PResult<ast::SimpleStmt> {
        trace!("parse_simple_stmt");
        unimplemented!()
    }
    fn parse_decl_stmt(&mut self) -> PResult<ast::SimpleStmt> {
        trace!("parse_decl_stmt");
        unimplemented!()
    }

    // XXX: error msg
    fn parse_expr(&mut self) -> PResult<ast::Expr> {
        trace!("parse_expr");

        self.parse_potential_binary_expr(0)
    }

    /// Parse a unary *expression*, which can be a primary expression OR a unary *operation*.
    fn parse_unary_expr(&mut self) -> PResult<ast::UnaryExpr> {
        // UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
        trace!("parse_unary_expr");

        if self.token.kind.is_unary_op() {
            Ok(ast::UnaryExpr::UnaryOperation(ast::UnaryOperation {
                operator: try!(self.parse_unary_operator()),
                operand: Box::new(try!(self.parse_unary_expr())),
            }))
        } else if self.token.kind == TokenKind::Operator(Operator::Arrow) {
            unimplemented!()
        } else {
            unimplemented!()
        }
    }

    fn parse_unary_operator(&mut self) -> PResult<ast::UnaryOperator> {
        unimplemented!()
    }

    fn parse_unary_operation(&mut self) -> PResult<ast::UnaryOperation> {
        trace!("parse_unary_operation");

        Ok(ast::UnaryOperation {
            operator: try!(self.parse_unary_operator()),
            operand: Box::new(try!(self.parse_unary_expr())),
        })
    }

    // This is pretty much a straight port from the official Go source.
    fn parse_potential_binary_expr(&mut self, prec1: i32) -> PResult<ast::Expr> {
        // Grammar:
        //  Expression binary_op Expression
        trace!("parse_potential_binary_expr");

        let mut x = ast::Expr::Unary(try!(self.parse_unary_expr()));

        loop {
            let op_kind = self.token.kind;
            let precedence = op_kind.precedence();
            if precedence < prec1 {
                return Ok(x);
            }

            self.bump();

            let y = try!(self.parse_potential_binary_expr(precedence + 1));
            x = ast::Expr::Binary(ast::BinaryExpr {
                lhs: Box::new(x),
                operator: op_kind,
                rhs: Box::new(y),
            });
        }
    }

    /// Parse an identifier.
    // XXX: come back later. String interning and all that.
    fn parse_ident(&mut self) -> PResult<String> {
        trace!("parse_ident");
        match self.token.kind {
            TokenKind::Ident => {
                Ok(self.bump_and_get()
                       .value
                       .clone()
                       .expect("BUG: missing value in identifier token"))
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
    fn parse_string_lit(&mut self) -> PResult<Vec<u8>> {
        trace!("parse_string_lit");
        // Grammar:
        //
        // ```
        // string_lit             = raw_string_lit | interpreted_string_lit .
        // raw_string_lit         = "`" { unicode_char | newline } "`" .
        // interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
        // ```

        match self.token.kind {
            TokenKind::Literal(Literal::Str) => {
                // Interpret the string.
                let raw_val = self.bump_and_get().value.expect("BUG: missing Str value");
                Ok(try!(self.interpret_string_lit(raw_val)))
            }
            TokenKind::Literal(Literal::StrRaw) => {
                // Only interpreting that needs to be done is removing carriage returns.
                let raw_val = self.bump_and_get().value.expect("BUG: missing StrRaw value");
                let mut byte_vec = raw_val.into_bytes();
                byte_vec.retain(|&c| c != b'\r');
                Ok(byte_vec)
            }
            _ => {
                Err(self.err(ErrorKind::unexpected_token(
                        vec![TokenKind::Literal(Literal::Str), TokenKind::Literal(Literal::StrRaw)],
                        self.token.clone())))
            }
        }
    }

    fn interpret_unicode_escape(&self,
                                long: bool,
                                chars: &mut CharIndices,
                                buf: &mut Vec<u8>)
                                -> PResult<()> {
        let num_digits = if long {
            8
        } else {
            4
        };

        let mut value = 0u32;

        for _ in 0..num_digits {
            if let Some((_, c)) = chars.next() {
                if let Some(number) = c.to_digit(16) {
                    value *= 16;
                    value += number;
                } else {
                    let msg = format!("illegal character in unicode escape: '{}'", c);
                    return Err(self.err(ErrorKind::other(msg)));
                }
            } else {
                let msg = "unexpected end of string in unicode escape";
                return Err(self.err(ErrorKind::other(msg)));
            }
        }

        return if let Some(value_char) = ::std::char::from_u32(value) {
            let mut tmp = String::with_capacity(4);
            tmp.push(value_char);
            buf.extend_from_slice(tmp.as_bytes());
            Ok(())
        } else {
            let msg = format!("escape sequence is invalid unicode codepoint: {:#x}", value);
            Err(self.err(ErrorKind::other(msg)))
        };
    }

    fn interpret_octal_escape(&self, chars: &mut CharIndices, buf: &mut Vec<u8>) -> PResult<()> {
        let mut value = 0u16;

        for _ in 0..3 {
            if let Some((_, c)) = chars.next() {
                if let Some(number) = c.to_digit(8) {
                    value *= 8;
                    value += number as u16;
                } else {
                    let msg = format!("illegal character in octal escape: '{}'", c);
                    return Err(self.err(ErrorKind::other(msg)));
                }
            } else {
                let msg = "unexpected end of string in octal escape";
                return Err(self.err(ErrorKind::other(msg)));
            }
        }

        return if value > 255 {
            let msg = format!("illegal octal escape value > 255: {}", value);
            Err(self.err(ErrorKind::other(msg)))
        } else {
            buf.push(value as u8);
            Ok(())
        };
    }

    fn interpret_hex_escape(&self, chars: &mut CharIndices, buf: &mut Vec<u8>) -> PResult<()> {
        let mut value = 0u8;

        for _ in 0..2 {
            if let Some((_, c)) = chars.next() {
                if let Some(number) = c.to_digit(16) {
                    value *= 16;
                    value += number as u8;
                } else {
                    let msg = format!("illegal character in hex escape: '{}'", c);
                    return Err(self.err(ErrorKind::other(msg)));
                }
            } else {
                let msg = "unexpected end of string in hex escape";
                return Err(self.err(ErrorKind::other(msg)));
            }
        }

        buf.push(value);

        Ok(())
    }

    fn get_simple_escape(&self, c: char) -> Option<u8> {
        // The escapes are roughly sorted by most common first.
        // XXX: actually find out exact ordering
        let simple_escapes = [('\\', b'\\'),
                              ('n', b'\n'),
                              ('t', b'\t'),
                              ('v', b'\x0b'),
                              ('r', b'\r'),
                              ('b', b'\x08'),
                              ('f', b'\x0c'),
                              ('a', b'\x07')];

        return if let Some(escape) = simple_escapes.iter().find(|escape| escape.0 == c) {
            Some(escape.1)
        } else {
            None
        };
    }

    /// Interpret a string literal, converting escape patterns to bytes.
    /// Note that it returns a Vec<u8> rather than a String. This is because Go string literals
    /// are allowed to contain invalid ASCII/UTF-8 through the use of octal and hex escapes.
    fn interpret_string_lit(&mut self, lit: String) -> PResult<Vec<u8>> {
        let mut result = Vec::new();

        let mut char_indices = lit.char_indices();

        while let Some((offset, c)) = char_indices.next() {
            if c == '\\' {
                // A string literal with a value ending with \ shouldn't get past the lexer.
                let (_, c) = char_indices.next()
                                         .expect("unexpected end of string: this is a bug!");

                // First check to see if we have a simple escape.
                if let Some(escape_byte) = self.get_simple_escape(c) {
                    result.push(escape_byte);
                } else if c == '"' {
                    // \" is only valid in strings
                    result.push(b'"');
                } else if c == 'x' {
                    try!(self.interpret_hex_escape(&mut char_indices, &mut result));
                } else if c == 'u' || c == 'U' {
                    try!(self.interpret_unicode_escape(c == 'U', &mut char_indices, &mut result));
                } else if c.is_digit(8) {
                    try!(self.interpret_octal_escape(&mut char_indices, &mut result));
                } else {
                    let msg = format!("unknown escape sequence: {}", c);
                    return Err(self.err(ErrorKind::other(msg)));
                }
            } else {
                let orig_str = &lit[offset..offset + c.len_utf8()];
                result.extend_from_slice(orig_str.as_bytes());
            }
        }

        Ok(result)
    }
}

pub fn parse_tokens(tokens: Vec<TokenAndSpan>) -> ast::SourceFile {
    let parser = Parser::new(tokens.into_iter());
    // XXX: unwrapping
    parser.parse().unwrap()
}
