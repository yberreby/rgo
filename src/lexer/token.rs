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
        if let Token::Ident(_) = *self {
            true
        } else {
            false
        }
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
        unimplemented!()
    }

    pub fn can_start_send_stmt(&self) -> bool {
        unimplemented!()
    }

    pub fn can_start_inc_dec_stmt(&self) -> bool {
        unimplemented!()
    }

    pub fn can_start_assignment(&self) -> bool {
        unimplemented!()
    }

    pub fn can_start_short_var_decl(&self) -> bool {
        unimplemented!()
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
