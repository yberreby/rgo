#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Literal(Literal),
    Keyword(Keyword),
    OpenDelim(DelimToken),
    CloseDelim(DelimToken),
    // Binary operators.
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Star,
    /// /
    Slash,
    /// %
    Percent,
    /// &
    And,
    /// |
    Or,
    /// ^
    Caret,
    /// <<
    Lshift,
    /// >>
    Rshift,
    /// &^
    BitClear,
    // Compound operators.
    /// ++
    Increment,
    /// --
    Decrement,
    /// +=
    PlusAssign,
    /// -=
    MinusAssign,
    /// *=
    StarAssign,
    /// /=
    SlashAssign,
    /// %=
    PercentAssign,
    /// &=
    AndAssign,
    /// |=
    OrAssign,
    /// ^=
    CaretAssign,
    /// <<=
    LshiftAssign,
    /// >>=
    RshiftAssign,
    /// &^=
    BitClearAssign,
    // Boolean operators.
    /// !
    Not,
    /// &&
    AndAnd,
    /// ||
    OrOr,
    /// ==
    Equals,
    /// !=
    NotEqual,
    /// <
    LessThan,
    /// >
    GreaterThan,
    /// <=
    LessThanOrEqual,
    /// >=
    GreaterThanOrEqual,
    // Miscellaneous operators and tokens.
    /// =
    Assign,
    /// :=
    ColonAssign,
    /// <-
    ChanReceive,
    /// ...
    Ellipsis,
    /// ,
    Comma,
    /// .
    Dot,
    /// ;
    Semicolon,
    /// :
    Colon,
}

// XXX/TODO: come back here and review this code.

impl Token {
    #[inline]
    pub fn is_keyword(&self, kw: Keyword) -> bool {
        *self == Token::Keyword(kw)
    }

    #[inline]
    pub fn is_ident(&self) -> bool {
        if let Token::Ident(_) = *self {
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn is_unary_op(&self) -> bool {
        // unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
        match *self {
            Token::Plus |
            Token::Minus |
            Token::Not |
            Token::Caret |
            Token::Star |
            Token::And |
            Token::ChanReceive => true,
            _ => false,
        }
    }

    #[inline]
    pub fn can_start_statement(&self) -> bool {
        // Grammar:
        // Statement =
        //      Declaration | LabeledStmt | SimpleStmt |
        //      GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
        //      FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
        //      DeferStmt .
        //
        // SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
        self.can_start_decl() || self.can_start_labeled_stmt() || self.can_start_simple_stmt() ||
        self.can_start_go_stmt() ||
        self.can_start_block() || self.is_keyword(Keyword::If) ||
        // XXX/TODO: double check this is correct.
        self.is_keyword(Keyword::Switch) || self.is_keyword(Keyword::Select) ||
        self.is_keyword(Keyword::For) || self.is_keyword(Keyword::Defer)
    }

    pub fn can_start_block(&self) -> bool {
        *self == Token::OpenDelim(DelimToken::Brace)
    }

    pub fn can_start_return_stmt(&self) -> bool {
        self.is_keyword(Keyword::Return)
    }

    pub fn can_start_labeled_stmt(&self) -> bool {
        // LabeledStmt = Label ":" Statement .
        // Label       = identifier .
        self.is_ident()
    }

    pub fn can_start_go_stmt(&self) -> bool {
        self.is_keyword(Keyword::Go)
    }

    pub fn can_start_decl(&self) -> bool {
        // Declaration   = ConstDecl | TypeDecl | VarDecl .
        self.is_keyword(Keyword::Const) || self.is_keyword(Keyword::Type) ||
        self.is_keyword(Keyword::Var)
    }

    pub fn can_start_simple_stmt(&self) -> bool {
        *self == Token::Semicolon || self.can_start_expr() || self.can_start_send_stmt() ||
        self.can_start_inc_dec_stmt() || self.can_start_assignment() ||
        self.can_start_short_var_decl()
    }

    pub fn can_start_expr(&self) -> bool {
        // Expression = UnaryExpr | Expression binary_op Expression .
        // UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
        //
        // binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
        // rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
        // add_op     = "+" | "-" | "|" | "^" .
        // mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
        //
        // unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
        //
        //
        // PrimaryExpr =
        // 	Operand |
        // 	Conversion |
        // 	PrimaryExpr Selector |
        // 	PrimaryExpr Index |
        // 	PrimaryExpr Slice |
        // 	PrimaryExpr TypeAssertion |
        // 	PrimaryExpr Arguments .
        //
        // Selector       = "." identifier .
        // Index          = "[" Expression "]" .
        // Slice          = "[" ( [ Expression ] ":" [ Expression ] ) |
        //                      ( [ Expression ] ":" Expression ":" Expression )
        //                  "]" .
        // TypeAssertion  = "." "(" Type ")" .
        // Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
        //
        // Conversion = Type "(" Expression [ "," ] ")" .
        //
        // MethodExpr    = ReceiverType "." MethodName .
        // ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .

        // XXX/TODO: review this code - critical.
        self.can_start_unary_expr() || self.can_start_expr()
    }

    pub fn can_start_unary_expr(&self) -> bool {
        self.can_start_primary_expr() || self.is_unary_op()
    }

    pub fn can_start_primary_expr(&self) -> bool {
        self.can_start_operand() || self.can_start_conversion()
    }

    pub fn can_start_operand(&self) -> bool {
        // Operand     = Literal | OperandName | MethodExpr | "(" Expression ")" .
        // OperandName = identifier | QualifiedIdent.
        // MethodExpr    = ReceiverType "." MethodName .
        // ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .
        //
        // QualifiedIdent starts with an identifier.
        // So does MethodExpr.
        self.can_start_lit() || self.is_ident() || *self == Token::OpenDelim(DelimToken::Paren)
    }

    pub fn can_start_conversion(&self) -> bool {
        self.can_start_type()
    }

    pub fn can_start_type(&self) -> bool {
        unimplemented!()
    }

    pub fn can_start_lit(&self) -> bool {
        // Literal     = BasicLit | CompositeLit | FunctionLit .
        // BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
        self.can_start_basic_lit() || self.can_start_composite_lit() || self.can_start_func_lit()
    }

    pub fn can_start_basic_lit(&self) -> bool {
        if let Token::Literal(_) = *self {
            true
        } else {
            false
        }
    }

    pub fn can_start_composite_lit(&self) -> bool {
        // CompositeLit  = LiteralType LiteralValue .
        self.can_start_lit_type()
    }

    pub fn can_start_lit_type(&self) -> bool {
        // LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
        //                 SliceType | MapType | TypeName .
        self.can_start_struct_type() || self.can_start_array_type() ||
        *self == Token::OpenDelim(DelimToken::Bracket) || self.can_start_slice_type() ||
        self.can_start_map_type() || self.is_ident()
    }

    pub fn can_start_func_lit(&self) -> bool {
        unimplemented!()
    }

    pub fn can_start_struct_type(&self) -> bool {
        self.is_keyword(Keyword::Struct)
    }

    pub fn can_start_array_type(&self) -> bool {
        *self == Token::OpenDelim(DelimToken::Bracket)
    }

    pub fn can_start_slice_type(&self) -> bool {
        *self == Token::OpenDelim(DelimToken::Bracket)
    }

    pub fn can_start_map_type(&self) -> bool {
        self.is_keyword(Keyword::Map)
    }

    pub fn can_start_send_stmt(&self) -> bool {
        // SendStmt = Channel "<-" Expression .
        // Channel  = Expression .
        self.can_start_expr()
    }

    pub fn can_start_inc_dec_stmt(&self) -> bool {
        // IncDecStmt = Expression ( "++" | "--" ) .
        self.can_start_expr()
    }

    pub fn can_start_assignment(&self) -> bool {
        // Assignment = ExpressionList assign_op ExpressionList .
        // ExpressionList = Expression { "," Expression } .
        self.can_start_expr()
    }

    pub fn can_start_short_var_decl(&self) -> bool {
        // ShortVarDecl = IdentifierList ":=" ExpressionList .
        // IdentifierList = identifier { "," identifier } .
        self.is_ident()
    }
}

/// Opening or closing delimiter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DelimToken {
    /// A round parenthesis: `(` or `)`
    Paren,
    /// A square bracket: `[` or `]`
    Bracket,
    /// A curly brace: `{` or `}`
    Brace,
}

/// Reserved keywords.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Break,
    Case,
    Chan,
    Const,
    Continue,
    Default,
    Defer,
    Else,
    Fallthrough,
    For,
    Func,
    Go,
    Goto,
    If,
    Import,
    Interface,
    Map,
    Package,
    Range,
    Return,
    Select,
    Struct,
    Switch,
    Type,
    Var,
}

/// Literal of any kind.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    /// Integer literal.
    Integer(String),
    /// Floating-point literal.
    Float(String),
    /// Imaginary literal (e.g. `6.67428e-11i`).
    Imaginary(String),
    /// Rune literal (e.g. `'本'`, `'\U00101234'`).
    Rune(String),
    /// Interpreted string literal.
    Str(String),
    /// Raw string literal.
    StrRaw(String),
}
