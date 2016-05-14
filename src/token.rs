use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenAndSpan {
    pub token: Token,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Option<String>,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenKind::*;

        if self.kind == Ident || self.kind.is_literal() {
            write!(f, "{}", self.value.as_ref().unwrap())
        } else {
            fmt::Debug::fmt(&self.kind, f) // FIXME: we're just using the Debug impl
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,

    // Delimiters.
    /// (
    LParen,
    /// )
    RParen,
    /// [
    LBracket,
    /// ]
    RBracket,
    /// {
    LBrace,
    /// }
    RBrace,

    // Literals.
    /// Decimal integer literal.
    Decimal,
    /// Octal integer literal.
    Octal,
    /// Hex integer literal.
    Hex,
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
    // Operators.
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
    /// !
    Not,
    /// =
    Assign,
    /// :=
    ColonAssign,
    /// <-
    Arrow,

    // Misc.
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


impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}


impl TokenKind {
    pub fn precedence(self) -> i32 {
        // Precedence    Operator
        //    5             *  /  %  <<  >>  &  &^
        //    4             +  -  |  ^
        //    3             ==  !=  <  <=  >  >=
        //    2             &&
        //    1             ||
        use self::TokenKind::*;
        match self {
            Star | Slash | Percent | Lshift | Rshift | And | BitClear => 5,
            Plus | Minus | Or | Caret => 4,
            Equals | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => 3,
            AndAnd => 2,
            OrOr => 1,
            _ => panic!("BUG: calling .precedence() on a token which is not a binary operator"),
        }
    }

    pub fn is_ident(self) -> bool {
        self == TokenKind::Ident
    }

    pub fn is_unary_op(self) -> bool {
        // unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
        use self::TokenKind::*;
        match self {
            Plus | Minus | Not | Caret | Star | And | Arrow => true,
            _ => false,
        }
    }

    pub fn is_literal(self) -> bool {
        use self::TokenKind::*;
        match self {
            Str | StrRaw | Decimal | Octal | Hex | Float | Imaginary | Rune => true,
            _ => false,
        }
    }

    pub fn can_start_statement(self) -> bool {
        // Grammar:
        // Statement =
        //      Declaration | LabeledStmt | SimpleStmt |
        //      GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
        //      FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
        //      DeferStmt .
        //
        // SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment |
        // ShortVarDecl .

        if self.can_start_decl() || self.can_start_labeled_stmt() ||
           self.can_start_simple_stmt() || self.can_start_go_stmt() ||
           self.can_start_block() {
            return true;
        }

        use self::TokenKind::*;

        match self {
            Return | Break | Continue | Goto | Fallthrough | If |
            // XXX/TODO: double check this is correct.
            Switch | Select | For | Defer => true,
            _ => false,
        }
    }

    pub fn can_start_block(self) -> bool {
        self == TokenKind::LBrace
    }

    pub fn can_start_return_stmt(self) -> bool {
        self == TokenKind::Return
    }

    pub fn can_start_labeled_stmt(self) -> bool {
        // LabeledStmt = Label ":" Statement .
        // Label       = identifier .
        self.is_ident()
    }

    pub fn can_start_go_stmt(self) -> bool {
        self == TokenKind::Go
    }

    pub fn can_start_decl(self) -> bool {
        trace!("can_start_decl");
        // Declaration   = ConstDecl | TypeDecl | VarDecl .
        self == TokenKind::Const || self == TokenKind::Type || self == TokenKind::Var
    }

    pub fn can_start_simple_stmt(self) -> bool {
        self == TokenKind::Semicolon || self.can_start_expr() || self.can_start_send_stmt() ||
        self.can_start_inc_dec_stmt() || self.can_start_assignment() ||
        self.can_start_short_var_decl()
    }

    pub fn can_start_expr(self) -> bool {
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

    pub fn can_start_unary_expr(self) -> bool {
        self.can_start_primary_expr() || self.is_unary_op()
    }

    pub fn can_start_primary_expr(self) -> bool {
        self.can_start_operand() || self.can_start_conversion()
    }

    pub fn can_start_operand(self) -> bool {
        // Operand     = Literal | OperandName | MethodExpr | "(" Expression ")" .
        // OperandName = identifier | QualifiedIdent.
        // MethodExpr    = ReceiverType "." MethodName .
        // ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .
        //
        // QualifiedIdent starts with an identifier.
        // So does MethodExpr.
        self.can_start_lit() || self.is_ident() || self == TokenKind::LParen
    }

    pub fn can_start_conversion(self) -> bool {
        self.can_start_type()
    }

    pub fn can_start_type(self) -> bool {
        // Type      = TypeName | TypeLit | "(" Type ")" .
        // TypeName  = identifier | QualifiedIdent .
        // TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
        //      SliceType | MapType | ChannelType .
        self.is_ident() || self.can_start_type_lit() || self == TokenKind::LParen
    }

    pub fn can_start_type_lit(self) -> bool {
        // TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
        //             SliceType | MapType | ChannelType .
        self.can_start_array_type() || self.can_start_struct_type() ||
        self.can_start_pointer_type() || self.can_start_func_type() ||
        self.can_start_interface_type() || self.can_start_slice_type() ||
        self.can_start_map_type() || self.can_start_chan_type()
    }

    pub fn can_start_pointer_type(self) -> bool {
        self == TokenKind::Star
    }

    pub fn can_start_func_type(self) -> bool {
        // FunctionType   = "func" Signature .
        self == TokenKind::Func
    }

    pub fn can_start_interface_type(self) -> bool {
        // InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
        self == TokenKind::Interface
    }

    pub fn can_start_chan_type(self) -> bool {
        // ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
        self == TokenKind::Chan || self == TokenKind::Arrow
    }

    pub fn can_start_lit(self) -> bool {
        // Literal     = BasicLit | CompositeLit | FunctionLit .
        // BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
        self.can_start_basic_lit() || self.can_start_composite_lit() || self.can_start_func_lit()
    }

    pub fn can_start_basic_lit(self) -> bool {
        self.is_literal()
    }

    pub fn can_start_composite_lit(self) -> bool {
        // CompositeLit  = LiteralType LiteralValue .
        self.can_start_lit_type()
    }

    pub fn can_start_lit_type(self) -> bool {
        // LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
        //                 SliceType | MapType | TypeName .
        self.can_start_struct_type() || self.can_start_array_type() ||
        self == TokenKind::RBracket || self.can_start_slice_type() ||
        self.can_start_map_type() || self.is_ident()
    }

    pub fn can_start_func_lit(self) -> bool {
        // FunctionLit = "func" Function .
        self == TokenKind::Func
    }

    pub fn can_start_struct_type(self) -> bool {
        self == TokenKind::Struct
    }

    pub fn can_start_array_type(self) -> bool {
        self == TokenKind::RBracket
    }

    pub fn can_start_slice_type(self) -> bool {
        self == TokenKind::RBracket
    }

    pub fn can_start_map_type(self) -> bool {
        self == TokenKind::Map
    }

    pub fn can_start_send_stmt(self) -> bool {
        // SendStmt = Channel "<-" Expression .
        // Channel  = Expression .
        self.can_start_expr()
    }

    pub fn can_start_inc_dec_stmt(self) -> bool {
        // IncDecStmt = Expression ( "++" | "--" ) .
        self.can_start_expr()
    }

    pub fn can_start_assignment(self) -> bool {
        // Assignment = ExpressionList assign_op ExpressionList .
        // ExpressionList = Expression { "," Expression } .
        self.can_start_expr()
    }

    pub fn can_start_short_var_decl(self) -> bool {
        // ShortVarDecl = IdentifierList ":=" ExpressionList .
        // IdentifierList = identifier { "," identifier } .
        self.is_ident()
    }
}
