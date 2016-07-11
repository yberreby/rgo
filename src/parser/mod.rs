use std::iter::Peekable;
use num::bigint::BigInt;
use num::BigRational;
use token::*;
use ast;

#[cfg(test)]
mod test;

mod error;
pub use self::error::{PResult, Error, ErrorKind};

macro_rules! span {
    ($s:expr, $x:expr) => {{
        let start_off = $s.span.start;
        let val = $x;
        let end_off = $s.prev_end_offset;

        Spanned::new(Span {start: start_off, end: end_off}, val)
    }}
}

macro_rules! try_span {
    ($s:expr, $x:expr) => {
        span!($s, try!($x))
    }
}

pub struct Parser<R: Iterator<Item = TokenAndSpan>> {
    /// Our source of tokens.
    /// Users can choose to read all the tokens up-front, or to read them lazily.
    reader: Peekable<R>,
    /// The current token.
    token: Token,
    /// The span of the current token.
    span: Span,
    /// Byte offset of the end of the most recently consumed token.
    prev_end_offset: u32,
    /// (XXX) Seems to indicate the current level of nesting when parsing expressions.
    expression_level: u32,
}

impl<R: Iterator<Item = TokenAndSpan>> Parser<R> {
    pub fn new(mut it: R) -> Parser<R> {
        // TODO: handle missing tok gracefully.
        let first_tok_and_pos = it.next().expect("missing first token");
        debug!("first_tok_and_pos: {:?}", first_tok_and_pos);
        Parser {
            token: first_tok_and_pos.token,
            span: first_tok_and_pos.span,
            prev_end_offset: first_tok_and_pos.span.end,
            reader: it.peekable(),
            // In the Go code, the initial value of `exprLev` implicitly defaults to 0.
            expression_level: 0,
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
    #[cold]
    #[inline(never)]
    fn err(&self, kind: ErrorKind) -> Error {
        Error {
            span: self.span,
            kind: kind,
        }
    }

    /// Advance the parser by one token.
    fn bump(&mut self) {
        trace!("current token: {:?}", self.token);
        trace!("current span: {:?}", self.span);
        trace!("bump");
        let next = self.reader.next();

        self.prev_end_offset = self.span.end;

        if let Some(TokenAndSpan { span, token }) = next {
            self.token = token;
            self.span = span;
        } else {
            // XXX what span to set?
            self.token = Token {
                kind: TokenKind::Eof,
                value: None,
            };
        }
    }

    /// Advance the parser by one token and return the bumped token.
    fn bump_and_get(&mut self) -> Token {
        // XXX(perf): clone; cloning is needed to let bump() see the previous token.
        let old_token = self.token.clone();
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

    fn eat_and_get(&mut self, expected: TokenKind) -> PResult<(Token)> {
        if self.token.kind != expected {
            return Err(self.err(ErrorKind::unexpected_token(vec![expected], self.token.clone())));
        }
        Ok(self.bump_and_get())
    }

    // === parse_*() functions ===

    /// Parse a package clause (e.g. `package main`).
    fn parse_package_clause(&mut self) -> PResult<String> {
        trace!("parse_package_clause");

        try!(self.eat(TokenKind::Package));

        let package_name = try!(self.parse_ident());
        try!(self.eat(TokenKind::Semicolon));
        Ok(package_name)
    }

    /// Parse any number of import declarations.
    fn parse_import_decls(&mut self) -> PResult<Vec<Spanned<ast::ImportDecl>>> {
        trace!("parse_import_decls");
        let mut decls = Vec::new();

        loop {
            match self.token.kind {
                TokenKind::Import => {
                    decls.push(try_span!(self, self.parse_import_decl()));
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
                            specs.push(try_span!(self, self.parse_import_spec()));
                        }
                    }
                }
            }
            // Short import (single ImportSpec).
            _ => specs.push(try_span!(self, self.parse_import_spec())),
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
        let path = try_span!(self, self.parse_string_lit());

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
            TokenKind::Func => {
                let fd = try!(self.parse_func_decl());
                decls.push(ast::TopLevelDecl::Func(fd));
            }
            _ => {
                let e = ErrorKind::unexpected_token(vec![TokenKind::Func], self.token.clone());
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

        try!(self.eat(TokenKind::Func));
        let name = try_span!(self, self.parse_ident());
        let signature = try!(self.parse_func_signature());

        let body = match self.token.kind {
            // This function has a body, parse it.
            TokenKind::LBrace => Some(try!(self.parse_block())),
            // No body.
            _ => None,
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
        trace!("parse_func_params");
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
                    try!(self.eat(TokenKind::Comma));
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
            TokenKind::LParen => {
                try!(self.eat(TokenKind::LParen));
                let typ = try!(self.parse_type());
                try!(self.eat(TokenKind::RParen));
                Ok(typ)
            }
            // If a Type starts with an identifier, it can only be a TypeName.
            TokenKind::Ident => {
                let id = try!(self.parse_maybe_qualified_ident());
                Ok(ast::Type::Plain(id))
            }
            _ => unimplemented!(),
        }
    }

    fn parse_maybe_qualified_ident(&mut self) -> PResult<ast::MaybeQualifiedIdent> {
        let part1 = try!(self.parse_ident());

        let name;
        let package;
        match self.token.kind {
            // This is a qualified identifier.
            // XXX: should we move that to a new function?
            // Qualified idents can only appear in:
            // - types (that's what we're parsing)
            // - operands.
            TokenKind::Dot => {
                // XXX: I don't like this pattern.
                try!(self.eat(TokenKind::Dot));
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

        Ok(ast::MaybeQualifiedIdent {
            package: package,
            name: name,
        })
    }

    fn parse_block(&mut self) -> PResult<ast::Block> {
        trace!("parse_block");
        // Grammar:
        // Block = "{" StatementList "}" .
        // StatementList = { Statement ";" } .
        try!(self.eat(TokenKind::LBrace));

        let mut statements = Vec::new();
        while self.token.kind.can_start_statement() {
            statements.push(try!(self.parse_statement()));
            try!(self.eat(TokenKind::Semicolon));
        }

        try!(self.eat(TokenKind::RBrace));
        Ok(ast::Block(statements))
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

        use token::TokenKind::*;
        Ok(match self.token.kind {
            Type | Var | Const => try!(self.parse_decl_stmt()).into(),
            Go => try!(self.parse_go_stmt()).into(),
            Defer => try!(self.parse_defer_stmt()).into(),
            Return => try!(self.parse_return_stmt()).into(),
            If => try!(self.parse_if_stmt()).into(),
            Switch => try!(self.parse_switch_stmt()).into(),
            Select => try!(self.parse_select_stmt()).into(),
            For => try!(self.parse_for_stmt()).into(),
            Break => try!(self.parse_break_stmt()).into(),
            Continue => try!(self.parse_continue_stmt()).into(),
            Goto => try!(self.parse_goto_stmt()).into(),
            Fallthrough => try!(self.parse_fallthrough_stmt()).into(),
            LBrace => try!(self.parse_block()).into(),
            RBrace => {
                // a semicolon may be omitted before a closing "}"
                ast::EmptyStmt.into()
            }
            // All simple statements start with something expression-like.
            t if t.can_start_expr() => try!(self.parse_simple_stmt()).into(),
            _ => panic!("unexpected token"),
        })
    }

    fn parse_go_stmt(&mut self) -> PResult<ast::GoStmt> {
        trace!("parse_go_stmt");

        try!(self.eat(TokenKind::Go));
        Ok(ast::GoStmt { call: try_span!(self, self.parse_expr()) })
    }

    fn parse_break_stmt(&mut self) -> PResult<ast::BreakStmt> {
        trace!("parse_break_stmt");

        try!(self.eat(TokenKind::Break));
        let label = if self.token.kind == TokenKind::Semicolon {
            None
        } else {
            Some(try_span!(self, self.parse_ident()))
        };

        Ok(ast::BreakStmt { label: label })
    }

    fn parse_continue_stmt(&mut self) -> PResult<ast::ContinueStmt> {
        trace!("parse_continue_stmt");

        try!(self.eat(TokenKind::Continue));
        let label = if self.token.kind == TokenKind::Semicolon {
            None
        } else {
            Some(try_span!(self, self.parse_ident()))
        };

        Ok(ast::ContinueStmt { label: label })
    }

    fn parse_goto_stmt(&mut self) -> PResult<ast::GotoStmt> {
        trace!("parse_goto_stmt");

        try!(self.eat(TokenKind::Goto));
        Ok(ast::GotoStmt { label: try_span!(self, self.parse_ident()) })
    }

    fn parse_fallthrough_stmt(&mut self) -> PResult<ast::FallthroughStmt> {
        trace!("parse_fallthrough_stmt");

        try!(self.eat(TokenKind::Fallthrough));
        Ok(ast::FallthroughStmt)
    }

    fn parse_defer_stmt(&mut self) -> PResult<ast::DeferStmt> {
        trace!("parse_defer_stmt");

        try!(self.eat(TokenKind::Defer));
        Ok(ast::DeferStmt { call: try_span!(self, self.parse_expr()) })
    }

    fn parse_return_stmt(&mut self) -> PResult<ast::ReturnStmt> {
        trace!("parse_return_stmt");
        try!(self.eat(TokenKind::Return));
        Ok(ast::ReturnStmt { expr: try_span!(self, self.parse_expr()) })
    }

    fn parse_if_stmt(&mut self) -> PResult<ast::IfStmt> {
        // IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
        trace!("parse_if_stmt");

        try!(self.eat(TokenKind::If));

        let stmt = try_span!(self, self.parse_simple_stmt());

        let before_stmt;
        let condition;

        if self.token.kind == TokenKind::Semicolon {
            // We have a complex if statment.
            self.bump();
            before_stmt = Some(stmt);

            condition = try_span!(self, self.parse_expr());
        } else {
            // We have a simple if statment.
            // What we just parsed must be an ExprStmt representing the condition.
            if let ast::SimpleStmt::Expr(expr) = stmt.item {
                condition = expr;
            } else {
                return Err(Error {
                    span: stmt.span,
                    kind: ErrorKind::other("invalid expression as if condition"),
                });
            }
            before_stmt = None;
        }

        let block = try!(self.parse_block());

        let opt_else = if self.token.kind == TokenKind::Else {
            self.bump();

            Some(Box::new(match self.token.kind {
                TokenKind::LBrace => ast::Else::Block(try!(self.parse_block())),
                TokenKind::If => ast::Else::If(try!(self.parse_if_stmt())),
                _ => {
                    let expected = vec![TokenKind::LBrace, TokenKind::If];
                    return Err(self.err(ErrorKind::unexpected_token(expected, self.token.clone())));
                }
            }))
        } else {
            None
        };

        Ok(ast::IfStmt {
            before_stmt: before_stmt,
            condition: condition,
            block: block,
            opt_else: opt_else,
        })
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
        // ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
        // Condition = Expression .
        //
        // ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
        // InitStmt = SimpleStmt .
        // PostStmt = SimpleStmt .
        //
        // RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
        trace!("parse_for_stmt");

        try!(self.eat(TokenKind::For));

        Ok(ast::ForStmt {
            header: try!(self.parse_for_header()),
            body: try!(self.parse_block()),
        })

    }

    fn parse_for_header(&mut self) -> PResult<ast::ForHeader> {
        trace!("parse_for_header");
        unimplemented!()
    }

    fn expr_list_to_ident_list(&self,
                               exprs: &Vec<Spanned<ast::Expr>>)
                               -> PResult<Vec<Spanned<String>>> {
        trace!("expr_list_to_ident_list");
        // XXX: better errors

        let mut idents = Vec::new();

        use ast::{Expr, UnaryExpr, PrimaryExpr, Operand};

        for expr in exprs {
            if let Expr::Unary(UnaryExpr::Primary(x)) = expr.item.clone() {
                if let PrimaryExpr::Operand(Operand::MaybeQualifiedIdent(mqident)) = *x {
                    if mqident.package.is_some() {
                        return Err(Error {
                            kind: ErrorKind::other("expected unqualified ident"),
                            span: expr.span,
                        });
                    }

                    idents.push(Spanned::new(expr.span, mqident.name));
                    continue;
                }
            }

            // didn't successfully turn the expr into an ident
            return Err(Error {
                kind: ErrorKind::other("expected ident"),
                span: expr.span,
            });
        }

        Ok(idents)
    }

    fn parse_simple_stmt(&mut self) -> PResult<ast::SimpleStmt> {
        // SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment |
        //  ShortVarDecl .
        //
        // EmptyStmt = .
        // ExpressionStmt = Expression .
        // SendStmt = Channel "<-" Expression .
        // Channel  = Expression .
        // IncDecStmt = Expression ( "++" | "--" ) .
        // Assignment = ExpressionList assign_op ExpressionList .
        // assign_op = [ add_op | mul_op ] "=" .
        //
        // ShortVarDecl = IdentifierList ":=" ExpressionList .
        trace!("parse_simple_stmt");

        let exprs = try!(self.parse_expr_list());

        if self.token.kind.is_assign_op() {
            let op = ast::BinaryOperator::from_token_kind_assign_op(self.bump_and_get().kind);
            return Ok(ast::SimpleStmt::Assignment(ast::Assignment {
                lhs: exprs,
                rhs: try!(self.parse_expr_list()),
                op: op,
            }));
        }

        if self.token.kind == TokenKind::ColonAssign {
            let idents = try!(self.expr_list_to_ident_list(&exprs));
            return Ok(ast::SimpleStmt::ShortVarDecl(ast::ShortVarDecl {
                lhs: idents,
                rhs: try!(self.parse_expr_list()),
            }));
        }

        let expr;

        // Now we have no possible SimpleStmt types left that start with a list of exprs.
        if exprs.len() > 1 {
            return Err(self.err(
                    ErrorKind::unexpected_token(vec![TokenKind::Assign], self.token.clone())
            ));
        } else if exprs.len() == 1 {
            // move exprs to prevent access later and avoid cloning
            expr = exprs.into_iter().next().unwrap();
        } else {
            panic!("BUG");
        }

        match self.token.kind {
            TokenKind::Arrow => {
                self.bump();
                Ok(ast::SimpleStmt::Send(ast::SendStmt {
                    channel: expr,
                    expr: try_span!(self, self.parse_expr()),
                }))
            }
            TokenKind::Increment => {
                self.bump();
                Ok(ast::SimpleStmt::IncDec(ast::IncDecStmt {
                    expr: expr,
                    is_dec: false,
                }))
            }
            TokenKind::Decrement => {
                self.bump();
                Ok(ast::SimpleStmt::IncDec(ast::IncDecStmt {
                    expr: expr,
                    is_dec: true,
                }))
            }
            TokenKind::Semicolon => Ok(ast::SimpleStmt::Expr(expr)),
            _ => {
                let expected = vec![TokenKind::Arrow,
                                    TokenKind::Increment,
                                    TokenKind::Decrement,
                                    TokenKind::Assign];
                Err(self.err(ErrorKind::unexpected_token(expected, self.token.clone())))
            }
        }
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

    fn parse_expr_list(&mut self) -> PResult<Vec<Spanned<ast::Expr>>> {
        // ExpressionList = Expression { "," Expression } .
        trace!("parse_expr_list");

        let mut res = Vec::new();

        res.push(try_span!(self, self.parse_expr()));

        while self.token.kind == TokenKind::Comma {
            self.bump();
            res.push(try_span!(self, self.parse_expr()));
        }

        Ok(res)
    }

    /// Parse a unary *expression*, which can be a primary expression OR a unary *operation*.
    // XXX: too much repetition, could probably be shortened
    fn parse_unary_expr(&mut self) -> PResult<ast::UnaryExpr> {
        // UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
        trace!("parse_unary_expr");

        match self.token.kind {
            // kind: unary operator
            TokenKind::Plus | TokenKind::Minus | TokenKind::Not | TokenKind::Caret |
            TokenKind::And => {
                let op = ast::UnaryOperator::from_token_kind(self.token.kind).expect("BUG");
                self.bump();
                let x = try_span!(self, self.parse_unary_expr());
                Ok(ast::UnaryExpr::UnaryOperation(ast::UnaryOperation {
                    operator: op,
                    operand: Box::new(x),
                }))
            }
            // channel receive operation
            TokenKind::Arrow => {
                self.bump();
                let x = try_span!(self, self.parse_unary_expr());
                Ok(ast::UnaryExpr::UnaryOperation(ast::UnaryOperation {
                    operator: ast::UnaryOperator::ChanReceive,
                    operand: Box::new(x),
                }))
            }

            // deref expression - star op
            TokenKind::Star => {
                self.bump();
                let x = try_span!(self, self.parse_unary_expr());
                Ok(ast::UnaryExpr::UnaryOperation(ast::UnaryOperation {
                    operator: ast::UnaryOperator::Deref,
                    operand: Box::new(x),
                }))
            }
            // No operator, this can only be a primary expression.
            _ => {
                let x = try!(self.parse_primary_expr());
                Ok(ast::UnaryExpr::Primary(Box::new(x)))
            }
        }
    }


    fn parse_selector_expr(&mut self, x: ast::ExprOrType) -> PResult<ast::SelectorExpr> {
        // ~ "assert x is an expr or a type... and not a "raw type" such as
        // [...]T".
        // I have _no_ idea what the author meant by that.

        let selector = try!(self.parse_ident());

        // Here's how things look at this point:
        //
        //    value.fieldOrMethod
        //      ^       ^
        //      |       |
        //      |       |
        //     `x`  `selector`
        //
        // We had already parsed the value (`x`), encountered a dot, and
        // immediately knew we had to look for an identifier - a plain
        // identifier, which cannot be qualified. This identifier is
        // `selector`.

        Ok(ast::SelectorExpr {
            operand: Box::new(x),
            selector: selector,
        })
    }

    fn parse_type_assertion(&mut self, x: ast::PrimaryExpr) -> PResult<ast::TypeAssertion> {
        // So now `x` _has_ to be an expression, and not a _type_. According to
        // the original Go source. Don't ask me!

        //    x.(T)
        //       ^
        //       |
        //       |
        //   left paren
        try!(self.eat(TokenKind::LParen));

        // XXX: left off spanning for now.

        let typ;

        // There's a catch. Type switches. Look at this code:
        //
        //    someVal.(type)
        //
        // This is the _literal_ `type` keyword. Not a placeholder. A keyword.
        // It indicates a special kind of switch that compares to types instead
        // of values.
        if self.token.kind == TokenKind::Type {
            typ = None;
            self.bump();
        } else {
            typ = Some(try!(self.parse_type()));
        }

        try!(self.eat(TokenKind::RParen));


        Ok(ast::TypeAssertion {
            expr: Box::new(x),
            typ: typ,
        })
    }

    // XXX: straight, unidiomatic port from Go source.
    // Well... almost. I tried to clean up a bit.
    // This method (and the corresponding smaller methods) is really complicated. It needs careful
    // review, and a lot of testing.
    fn parse_primary_expr(&mut self) -> PResult<ast::PrimaryExpr> {
        trace!("parse_primary_expr");

        // Parse the first part of the expression, which may be a literal, a (potentially
        // qualified) identifier, a method expression (= `SomeType.MethodName` - returns a
        // function), or any expression if it is parenthesized.
        //
        //    Operand     = Literal | OperandName | MethodExpr | "(" Expression ")" .
        //
        let mut x = try!(self.parse_operand());

        'L: loop {
            match self.token.kind {
                TokenKind::Dot => {
                    self.bump();

                    match self.token.kind {
                        TokenKind::Ident => {
                            let first_part = x.into_expr_or_type();
                            let selector = try!(self.parse_selector_expr(first_part));
                            x = ast::ExprOrType::Expr(ast::PrimaryExpr::SelectorExpr(selector));
                        }
                        TokenKind::LParen => {
                            let expr = x.into_expr();
                            let assertion = try!(self.parse_type_assertion(expr));
                            x = ast::ExprOrType::Expr(ast::PrimaryExpr::TypeAssertion(assertion));
                        }
                        _ => {
                            // XXX: the Go parser signals the error and keeps going, here.
                            // XXX: needs better diagnostic to explain we were looking for a
                            // selector or a type assertion.
                            let expected = vec![TokenKind::Ident, TokenKind::LParen];
                            return Err(self.err(
                                    ErrorKind::unexpected_token(expected, self.token.clone())
                            ));
                        }
                    }
                }
                TokenKind::LBracket => {
                    // ~"x must be an expression, not a type"
                    let expr = x.into_expr();
                    x = try!(self.parse_index_or_slice(x));
                }
                TokenKind::LParen => {
                    // ~"x must be an expression or a type, but NOT a 'raw type'"...?

                    // `x(x)` could be `T(x)` (conversion) or `f(x)` (function call).
                    let callee = x.into_expr_or_type();
                    x = try!(self.parse_call_or_conversion(x));
                }
                TokenKind::LBrace => {
                    if x.is_literal_type() && (self.expression_level >= 0 || !x.is_type_name()) {
                        x = try!(self.parse_literal_value(x));
                    }
                }
                _ => break 'L,
            }
        }

        Ok(x)
    }

    fn parse_literal_value(&mut self) -> PResult<ast::LiteralValue> {
        trace!("parse_literal_value");

        try!(self.eat(TokenKind::LBrace));
        let mut elems = None;

        self.expression_level += 1;
        if self.token.kind != TokenKind::RBrace {
            elems = Some(try!(self.parse_keyed_literal_element_list()));
        }
        self.expression_level -= 1;

        try!(self.eat(TokenKind::RBrace));

        Ok(ast::LiteralValue { elems: elems })
    }

    fn parse_keyed_literal_element_list(&mut self) -> PResult<Vec<ast::KeyedLiteralElem>> {
        trace!("parse_keyed_literal_element_list");

        let mut v = Vec::new();

        // XXX: original code:
        //      for p.tok != token.RBRACE && p.tok != token.EOF {
        // Do we need these checks?
        while self.token.kind != TokenKind::RBrace && self.token.kind != TokenKind::Eof {
            let elem = try!(self.parse_keyed_literal_element());
            v.push(elem);

            // XXX: original code:
            //      if !p.atComma("composite literal", token.RBRACE) {
            if self.token.kind == TokenKind::Comma {
                self.bump();
            } else {
                break;
            }
        }

        Ok(v)
    }

    fn parse_keyed_literal_element(&mut self) -> PResult<ast::KeyedLiteralElem> {
        let mut key = None;
        let mut elem = try_span!(self, self.parse_inner_literal_value());

        if self.token.kind == TokenKind::Colon {
            self.bump();
            key = Some(elem);
            elem = try_span!(self, self.parse_inner_literal_value());
        }

        Ok(ast::KeyedLiteralElem {
            key: key,
            elem: elem,
        })
    }

    // XXX XXX XXX: Straight, untested, stupid port from the Go source.
    fn parse_operand(&mut self) -> PResult<ast::Operand> {
        trace!("parse_operand");

        let ret = match self.token.kind {
            TokenKind::Ident => {
                let id = try!(self.parse_maybe_qualified_ident());
                ast::Operand::MaybeQualifiedIdent(id)
            }
            token_kind if token_kind.can_start_basic_lit() => {
                let lit = try!(self.parse_basic_lit());
                ast::Operand::Lit(ast::Literal::Basic(lit))
            }
            TokenKind::LParen => {
                //    "(" Expr ")"
                //
                // Skip past the LParen.
                self.bump();
                self.expression_level += 1;
                let expr = try!(self.parse_expr());
                self.expression_level -= 1;
                try!(self.eat(TokenKind::RParen));
                ast::Operand::Expr(expr)
            }
            TokenKind::Func => {
                let fl = try!(self.parse_func_lit());
                ast::Operand::Lit(ast::Literal::Func(fl))
            }
            _ => {
                let method_expr = try!(self.parse_selector_expr());
                ast::Operand::MethodExpr(method_expr)
            }
        };

        Ok(ret)
    }

    fn parse_func_lit(&mut self) -> PResult<ast::FuncLit> {
        Ok(ast::FuncLit {
            signature: try!(self.parse_func_signature()),
            body: try!(self.parse_block()),
        })
    }

    fn parse_index_or_slice(&mut self, x: ast::PrimaryExpr) -> PResult<ast::PrimaryExpr> {
        unimplemented!()
    }

    fn parse_call_or_conversion(&mut self, callee: ast::ExprOrType) -> PResult<ast::CallOrConv> {
        trace!("parse_call_or_conversion");

        try!(self.eat(TokenKind::LParen));
        self.expression_level += 1;

        let mut args = Vec::new();
        let mut ellipsis = false;

        // XXX: no conditions, just a break.
        loop {
            let x = try!(self.parse_expr_or_type());
            args.push(x);

            if self.token.kind == TokenKind::Ellipsis {
                ellipsis = true;
                self.bump();
            }

            // DO NOT use an `else if` here, maybe we just bumped past the ellipsis.
            if self.token.kind == TokenKind::Comma {
                self.bump();
            } else {
                break;
            }
        }

        self.expression_level -= 1;
        try!(self.eat(TokenKind::RParen));

        Ok(ast::CallOrConv {
            callee: Box::new(callee),
            args: args,
            ellipsis: ellipsis,
        })
    }

    fn parse_basic_lit(&mut self) -> PResult<ast::BasicLit> {
        // BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
        trace!("parse_basic_lit");

        use token::TokenKind::*;

        match self.token.kind {
            Decimal | Octal | Hex => Ok(ast::BasicLit::Int(try!(self.parse_int_lit()))),
            Str | StrRaw => Ok(ast::BasicLit::Str(try!(self.parse_string_lit()))),
            Float => {
                let value = self.bump_and_get()
                    .value
                    .expect("BUG: missing value in float literal");
                Ok(ast::BasicLit::Float(try!(self.interpret_float_lit(&value[..],
                                                                      "float literal"))))
            }
            Imaginary => {
                let value = self.bump_and_get()
                    .value
                    .expect("BUG: missing value in imaginary literal");
                assert!(value.chars().last().unwrap() == 'i',
                        "BUG: imaginary literal token does not end with i");
                let value_ref = value.trim_right_matches('i');
                Ok(ast::BasicLit::Imaginary(try!(self.interpret_float_lit(value_ref,
                                                                          "imaginary literal"))))
            }
            Rune => Ok(ast::BasicLit::Rune(try!(self.parse_rune_lit()))),
            _ => {
                let expected = vec![Decimal, Octal, Hex, Float, Imaginary, Rune, Str, StrRaw];
                return Err(self.err(ErrorKind::unexpected_token(expected, self.token.clone())));
            }
        }
    }

    fn parse_rune_lit(&mut self) -> PResult<char> {
        // rune_lit         = "'" ( unicode_value | byte_value ) "'" .
        trace!("parse_rune_lit");

        let value = try!(self.eat_and_get(TokenKind::Rune))
            .value
            .expect("BUG: missing value in rune literal");

        let mut char_indices = value.char_indices().peekable();
        let result;

        let (_, c) = char_indices.next().unwrap();
        if c == '\\' {
            let &(_, pc) = char_indices.peek().expect("BUG: rune lit containing only \\");

            // First check to see if we have a simple escape.
            if let Some(escape_byte) = self.get_simple_escape(pc) {
                char_indices.next();
                result = escape_byte as char;
            } else if pc == '\'' {
                char_indices.next();
                // \' is only valid in runes
                result = '\'';
            } else if pc == 'x' {
                char_indices.next();
                result = try!(self.interpret_hex_escape(&mut char_indices)) as char;
            } else if pc == 'u' || pc == 'U' {
                char_indices.next();
                result = try!(self.interpret_unicode_escape(pc == 'U', &mut char_indices));
            } else if pc.is_digit(8) {
                result = try!(self.interpret_octal_escape(&mut char_indices)) as char;
            } else {
                let msg = format!("unknown escape sequence: {}", pc);
                return Err(self.err(ErrorKind::other(msg)));
            }
        } else if c == '\n' {
            return Err(self.err(ErrorKind::other("newline in rune literal")));
        } else {
            result = c;
        }

        if char_indices.next().is_none() {
            Ok(result)
        } else {
            Err(self.err(ErrorKind::other("multiple characters in rune literal")))
        }
    }

    /// Interpret the value of a float/imaginary literal and return the result as a BigRational.
    /// If this method is being used to parse an imaginary lit, don't include the trailing `i`.
    fn interpret_float_lit(&mut self, value: &str, token_name: &str) -> PResult<BigRational> {
        // float_lit = decimals "." [ decimals ] [ exponent ] |
        //             decimals exponent |
        //             "." decimals [ exponent ] .
        // decimals  = decimal_digit { decimal_digit } .
        // exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals .
        trace!("interpret_float_lit");

        let mut res = BigRational::from_integer(BigInt::from(0u8));
        let mut chars = value.chars().peekable();
        let mut parse_exponent = false;
        let mut digits_after_dot = 0u32; // the number of digits after the dot we are

        while let Some(c) = chars.next() {
            if c == '.' {
                digits_after_dot = 1;
            } else if c == 'e' || c == 'E' {
                parse_exponent = true;
                break;
            } else {
                let digit = c.to_digit(10).expect("BUG: invalid char in float/imag lit");
                let digit_value = BigRational::from_integer(BigInt::from(digit));

                if digits_after_dot == 0 {
                    res = res * BigRational::from_integer(BigInt::from(10u8));
                    res = res + BigRational::from_integer(BigInt::from(digit));
                } else {
                    res = res +
                          digit_value /
                          BigRational::from_integer(BigInt::from(10u32.pow(digits_after_dot)));

                    digits_after_dot += 1;
                }
            }
        }

        if parse_exponent {
            let mut negative = false;
            if let Some(&c) = chars.peek() {
                if c == '+' {
                    chars.next();
                } else if c == '-' {
                    negative = true;
                    chars.next();
                }
            } else {
                // Empty exponent
                return Err(self.err(ErrorKind::other(format!("malformed {} exponent",
                                                             token_name))));
            }

            let mut exponent = 0;
            while let Some(c) = chars.next() {
                exponent *= 10;
                exponent += c.to_digit(10).expect("BUG: invalid char in float/imag lit exponent");
            }

            for _ in 0..exponent {
                if negative {
                    res = res / BigRational::from_integer(BigInt::from(10u8));
                } else {
                    res = res * BigRational::from_integer(BigInt::from(10u8));
                }
            }
        }

        Ok(res)
    }

    fn parse_int_lit(&mut self) -> PResult<BigInt> {
        // int_lit     = decimal_lit | octal_lit | hex_lit .
        // decimal_lit = ( "1" … "9" ) { decimal_digit } .
        // octal_lit   = "0" { octal_digit } .
        // hex_lit     = "0" ( "x" | "X" ) hex_digit { hex_digit } .

        trace!("parse_int_lit");

        match self.token.kind {
            TokenKind::Decimal => {
                let value = self.bump_and_get().value.expect("BUG: missing value in decimal lit");
                Ok(try!(self.interpret_int(&value[..], 10, "decimal literal")))
            }
            TokenKind::Octal => {
                let value = self.bump_and_get().value.expect("BUG: missing value in octal lit");
                assert_eq!(value.chars().next(), Some('0'));
                Ok(try!(self.interpret_int(&value[1..], 8, "octal literal")))
            }
            TokenKind::Hex => {
                let value = self.bump_and_get().value.expect("BUG: missing value in hex lit");
                assert!(value.starts_with("0x") || value.starts_with("0X"));
                Ok(try!(self.interpret_int(&value[2..], 16, "hex literal")))
            }
            _ => {
                return Err(self.err(ErrorKind::unexpected_token(vec![TokenKind::Decimal,
                                                                     TokenKind::Octal,
                                                                     TokenKind::Hex],
                                                                self.token.clone())));
            }
        }
    }

    /// Interpret the value of an int literal and return the result as a BigInt, using the provided
    /// base.
    ///
    /// Use `token_name` to specify what type of literal this is, for error messages. To
    /// parse an octal or hex literal, do not pass the `0` or `0x` prefixes.
    fn interpret_int(&mut self, lit: &str, base: u32, token_name: &str) -> PResult<BigInt> {
        trace!("interpret_int");

        let mut res = BigInt::from(0u8);

        for c in lit.chars() {
            if let Some(d) = c.to_digit(base) {
                res = res * BigInt::from(base);
                res = res + BigInt::from(d);
            } else {
                let msg = format!("invalid character in {}: {}", token_name, c);
                return Err(self.err(ErrorKind::other(msg)));
            }
        }

        Ok(res)
    }

    // This is pretty much a straight port from the official Go source.
    fn parse_potential_binary_expr(&mut self, prec1: i32) -> PResult<ast::Expr> {
        // Grammar:
        //  Expression binary_op Expression
        trace!("parse_potential_binary_expr");

        let a = try_span!(self, self.parse_unary_expr());
        let mut x = Spanned::new(a.span, ast::Expr::Unary(a.item));

        loop {
            let bin_op_kind = match ast::BinaryOperator::from_token_kind(self.token.kind) {
                Some(x) => x,
                // The current token is not a binary operator.
                None => return Ok(x.item),
            };

            let precedence = bin_op_kind.precedence();
            if precedence < prec1 {
                return Ok(x.item);
            }

            self.bump();

            let y = try_span!(self, self.parse_potential_binary_expr(precedence + 1));
            let binop_span = Span {
                start: x.span.start,
                end: y.span.end,
            };
            x = Spanned::new(binop_span,
                             ast::Expr::Binary(ast::BinaryExpr {
                                 lhs: Box::new(x),
                                 op: bin_op_kind,
                                 rhs: Box::new(y),
                             }));
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
            TokenKind::Str => {
                // Interpret the string.
                let raw_val = self.bump_and_get().value.expect("BUG: missing Str value");
                Ok(try!(self.interpret_string_lit(raw_val)))
            }
            TokenKind::StrRaw => {
                // Only interpreting that needs to be done is removing carriage returns.
                let raw_val = self.bump_and_get().value.expect("BUG: missing StrRaw value");
                let mut byte_vec = raw_val.into_bytes();
                byte_vec.retain(|&c| c != b'\r');
                Ok(byte_vec)
            }
            _ => {
                Err(self.err(ErrorKind::unexpected_token(vec![TokenKind::Str, TokenKind::StrRaw],
                                                         self.token.clone())))
            }
        }
    }

    fn interpret_unicode_escape(&self,
                                long: bool,
                                chars: &mut Iterator<Item = (usize, char)>)
                                -> PResult<char> {
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
            Ok(value_char)
        } else {
            let msg = format!("escape sequence is invalid unicode codepoint: {:#x}", value);
            Err(self.err(ErrorKind::other(msg)))
        };
    }

    fn interpret_octal_escape(&self, chars: &mut Iterator<Item = (usize, char)>) -> PResult<u8> {
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
            Ok(value as u8)
        };
    }

    fn interpret_hex_escape(&self, chars: &mut Iterator<Item = (usize, char)>) -> PResult<u8> {
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

        Ok(value)
    }

    fn get_simple_escape(&self, c: char) -> Option<u8> {
        // The escapes are roughly sorted by most common first.
        // XXX: actually find out exact ordering
        // XXX(perf): .find() on a static array _will_ be slower than a match.
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

        let mut char_indices = lit.char_indices().peekable();

        while let Some((offset, c)) = char_indices.next() {
            if c == '\\' {
                // A string literal with a value ending with \ shouldn't get past the lexer.
                let &(_, pc) = char_indices.peek()
                    .expect("unexpected end of string: this is a bug!");

                // First check to see if we have a simple escape.
                if let Some(escape_byte) = self.get_simple_escape(pc) {
                    char_indices.next();
                    result.push(escape_byte);
                } else if pc == '"' {
                    char_indices.next();
                    // \" is only valid in strings
                    result.push(b'"');
                } else if pc == 'x' {
                    char_indices.next();
                    let byte = try!(self.interpret_hex_escape(&mut char_indices));
                    result.push(byte);
                } else if pc == 'u' || pc == 'U' {
                    char_indices.next();
                    let value_char = try!(self.interpret_unicode_escape(pc == 'U',
                                                                        &mut char_indices));

                    let mut tmp = String::with_capacity(4);
                    tmp.push(value_char);
                    result.extend_from_slice(tmp.as_bytes());
                } else if pc.is_digit(8) {
                    let byte = try!(self.interpret_octal_escape(&mut char_indices));
                    result.push(byte);
                } else {
                    let msg = format!("unknown escape sequence: {}", pc);
                    return Err(self.err(ErrorKind::other(msg)));
                }
            } else if c == '\n' {
                let msg = format!("newline in string");
                return Err(self.err(ErrorKind::other(msg)));
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
