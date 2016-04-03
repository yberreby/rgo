#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Literal(Literal),
    Keyword(Keyword),
    OpenDelim(DelimToken),
    CloseDelim(DelimToken),
    /// Significant whitespace, containing one or more newlines.
    Whitespace,
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
