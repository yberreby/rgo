#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    // Delimiters.
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    // Literals.
    /// Integer literal.
    Integer,
    /// Floating-point literal.
    Float,
    /// Imaginary literal (e.g. `6.67428e-11i`).
    Imaginary,
    /// Rune literal (e.g. `'本'`, `'\U00101234'`).
    Rune,
    /// Interpreted string literal.
    Str,
    /// Raw string literal.
    StrRaw,
    // Keywords.
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
    // -----
    //
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
    LeftArrow,
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
    /// End of file
    Eof,
}

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
    LeftArrow,
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
    /// End of file
    Eof,
}

// XXX/TODO: come back here and review this code.

impl Token {
    pub fn kind(&self) -> TokenKind {
        match *self {
            Token::Keyword(ref kw) => {
                match *kw {
                    Keyword::Break => TokenKind::Break,
                    Keyword::Case => TokenKind::Case,
                    Keyword::Chan => TokenKind::Chan,
                    Keyword::Const => TokenKind::Const,
                    Keyword::Continue => TokenKind::Continue,
                    Keyword::Default => TokenKind::Default,
                    Keyword::Defer => TokenKind::Defer,
                    Keyword::Else => TokenKind::Else,
                    Keyword::Fallthrough => TokenKind::Fallthrough,
                    Keyword::For => TokenKind::For,
                    Keyword::Func => TokenKind::Func,
                    Keyword::Go => TokenKind::Go,
                    Keyword::Goto => TokenKind::Goto,
                    Keyword::If => TokenKind::If,
                    Keyword::Import => TokenKind::Import,
                    Keyword::Interface => TokenKind::Interface,
                    Keyword::Map => TokenKind::Map,
                    Keyword::Package => TokenKind::Package,
                    Keyword::Range => TokenKind::Range,
                    Keyword::Return => TokenKind::Return,
                    Keyword::Select => TokenKind::Select,
                    Keyword::Struct => TokenKind::Struct,
                    Keyword::Switch => TokenKind::Switch,
                    Keyword::Type => TokenKind::Type,
                    Keyword::Var => TokenKind::Var,
                }
            }
            Token::Literal(ref lit) => {
                match *lit {
                    Literal::Integer(_) => TokenKind::Integer,
                    Literal::Float(_) => TokenKind::Float,
                    Literal::Imaginary(_) => TokenKind::Imaginary,
                    Literal::Rune(_) => TokenKind::Rune,
                    Literal::Str(_) => TokenKind::Str,
                    Literal::StrRaw(_) => TokenKind::StrRaw,
                }
            }
            Token::OpenDelim(ref d) => {
                match *d {
                    DelimToken::Paren => TokenKind::LParen,
                    DelimToken::Bracket => TokenKind::LBracket,
                    DelimToken::Brace => TokenKind::LBrace,
                }
            }
            Token::CloseDelim(ref d) => {
                match *d {
                    DelimToken::Paren => TokenKind::RParen,
                    DelimToken::Bracket => TokenKind::RBracket,
                    DelimToken::Brace => TokenKind::RBrace,
                }
            }
            Token::Ident(_) => TokenKind::Ident,
            Token::Plus => TokenKind::Plus,
            Token::Minus => TokenKind::Minus,
            Token::Star => TokenKind::Star,
            Token::Slash => TokenKind::Slash,
            Token::Percent => TokenKind::Percent,
            Token::And => TokenKind::And,
            Token::Or => TokenKind::Or,
            Token::Caret => TokenKind::Caret,
            Token::Lshift => TokenKind::Lshift,
            Token::Rshift => TokenKind::Rshift,
            Token::BitClear => TokenKind::BitClear,
            Token::Increment => TokenKind::Increment,
            Token::Decrement => TokenKind::Decrement,
            Token::PlusAssign => TokenKind::PlusAssign,
            Token::MinusAssign => TokenKind::MinusAssign,
            Token::StarAssign => TokenKind::StarAssign,
            Token::SlashAssign => TokenKind::SlashAssign,
            Token::PercentAssign => TokenKind::PercentAssign,
            Token::AndAssign => TokenKind::AndAssign,
            Token::OrAssign => TokenKind::OrAssign,
            Token::CaretAssign => TokenKind::CaretAssign,
            Token::LshiftAssign => TokenKind::LshiftAssign,
            Token::RshiftAssign => TokenKind::RshiftAssign,
            Token::BitClearAssign => TokenKind::BitClearAssign,
            Token::Not => TokenKind::Not,
            Token::AndAnd => TokenKind::AndAnd,
            Token::OrOr => TokenKind::OrOr,
            Token::Equals => TokenKind::Equals,
            Token::NotEqual => TokenKind::NotEqual,
            Token::LessThan => TokenKind::LessThan,
            Token::GreaterThan => TokenKind::GreaterThan,
            Token::LessThanOrEqual => TokenKind::LessThanOrEqual,
            Token::GreaterThanOrEqual => TokenKind::GreaterThanOrEqual,
            Token::Assign => TokenKind::Assign,
            Token::ColonAssign => TokenKind::ColonAssign,
            Token::LeftArrow => TokenKind::LeftArrow,
            Token::Ellipsis => TokenKind::Ellipsis,
            Token::Comma => TokenKind::Comma,
            Token::Dot => TokenKind::Dot,
            Token::Semicolon => TokenKind::Semicolon,
            Token::Colon => TokenKind::Colon,
            Token::Eof => TokenKind::Eof,
        }
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






impl TokenKind {
    pub fn is_ident(&self) -> bool {
        *self == TokenKind::Ident
    }

    pub fn is_unary_op(&self) -> bool {
        // unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
        match *self {
            TokenKind::Plus |
            TokenKind::Minus |
            TokenKind::Not |
            TokenKind::Caret |
            TokenKind::Star |
            TokenKind::And |
            TokenKind::LeftArrow => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match *self {
            TokenKind::Integer |
            TokenKind::Float |
            TokenKind::Imaginary |
            TokenKind::Rune |
            TokenKind::Str |
            TokenKind::StrRaw => true,
            _ => false,
        }
    }

    pub fn can_start_statement(&self) -> bool {
        // Grammar:
        // Statement =
        //      Declaration | LabeledStmt | SimpleStmt |
        //      GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
        //      FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
        //      DeferStmt .
        //
        // SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment |
        // ShortVarDecl .
        self.can_start_decl() || self.can_start_labeled_stmt() || self.can_start_simple_stmt() ||
        self.can_start_go_stmt() ||
        self.can_start_block() || *self == TokenKind::If ||
        // XXX/TODO: double check this is correct.
        *self == TokenKind::Switch || *self == TokenKind::Select ||
        *self == TokenKind::For || *self == TokenKind::Defer
    }

    pub fn can_start_block(&self) -> bool {
        *self == TokenKind::LBrace
    }

    pub fn can_start_return_stmt(&self) -> bool {
        *self == TokenKind::Return
    }

    pub fn can_start_labeled_stmt(&self) -> bool {
        // LabeledStmt = Label ":" Statement .
        // Label       = identifier .
        self.is_ident()
    }

    pub fn can_start_go_stmt(&self) -> bool {
        *self == TokenKind::Go
    }

    pub fn can_start_decl(&self) -> bool {
        trace!("can_start_decl");
        // Declaration   = ConstDecl | TypeDecl | VarDecl .
        *self == TokenKind::Const || *self == TokenKind::Type || *self == TokenKind::Var
    }

    pub fn can_start_simple_stmt(&self) -> bool {
        *self == TokenKind::Semicolon || self.can_start_expr() || self.can_start_send_stmt() ||
        self.can_start_inc_dec_stmt() ||
        self.can_start_assignment() || self.can_start_short_var_decl()
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
        // Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ ","
        // ] ] ")" .
        //
        // Conversion = Type "(" Expression [ "," ] ")" .
        //
        // MethodExpr    = ReceiverType "." MethodName .
        // ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .

        // XXX/TODO: review this code - critical.
        self.can_start_unary_expr()
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
        self.can_start_lit() || self.is_ident() || *self == TokenKind::LParen
    }

    pub fn can_start_conversion(&self) -> bool {
        self.can_start_type()
    }

    pub fn can_start_type(&self) -> bool {
        // Type      = TypeName | TypeLit | "(" Type ")" .
        // TypeName  = identifier | QualifiedIdent .
        // TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
        //      SliceType | MapType | ChannelType .
        self.is_ident() || self.can_start_type_lit() || *self == TokenKind::LParen
    }

    pub fn can_start_type_lit(&self) -> bool {
        // TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
        //             SliceType | MapType | ChannelType .
        self.can_start_array_type() || self.can_start_struct_type() ||
        self.can_start_pointer_type() || self.can_start_func_type() ||
        self.can_start_interface_type() || self.can_start_slice_type() ||
        self.can_start_map_type() || self.can_start_chan_type()
    }

    pub fn can_start_pointer_type(&self) -> bool {
        *self == TokenKind::Star
    }

    pub fn can_start_func_type(&self) -> bool {
        // FunctionType   = "func" Signature .
        *self == TokenKind::Func
    }

    pub fn can_start_interface_type(&self) -> bool {
        // InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
        *self == TokenKind::Interface
    }

    pub fn can_start_chan_type(&self) -> bool {
        // ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
        *self == TokenKind::Chan || *self == TokenKind::LeftArrow
    }

    pub fn can_start_lit(&self) -> bool {
        // Literal     = BasicLit | CompositeLit | FunctionLit .
        // BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
        self.can_start_basic_lit() || self.can_start_composite_lit() || self.can_start_func_lit()
    }

    pub fn can_start_basic_lit(&self) -> bool {
        self.is_literal()
    }

    pub fn can_start_composite_lit(&self) -> bool {
        // CompositeLit  = LiteralType LiteralValue .
        self.can_start_lit_type()
    }

    pub fn can_start_lit_type(&self) -> bool {
        // LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
        //                 SliceType | MapType | TypeName .
        self.can_start_struct_type() || self.can_start_array_type() ||
        *self == TokenKind::RBracket || self.can_start_slice_type() ||
        self.can_start_map_type() || self.is_ident()
    }

    pub fn can_start_func_lit(&self) -> bool {
        // FunctionLit = "func" Function .
        *self == TokenKind::Func
    }

    pub fn can_start_struct_type(&self) -> bool {
        *self == TokenKind::Struct
    }

    pub fn can_start_array_type(&self) -> bool {
        *self == TokenKind::RBracket
    }

    pub fn can_start_slice_type(&self) -> bool {
        *self == TokenKind::RBracket
    }

    pub fn can_start_map_type(&self) -> bool {
        *self == TokenKind::Map
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
