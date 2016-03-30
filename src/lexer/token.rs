#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Literal(Literal),
    Keyword(Keyword),
    OpenDelim(DelimToken),
    CloseDelim(DelimToken),
    // XXX: not sure whether it's a good think to tokenize _all_ whitespace.
    // I'm doing it because I don't want to tie tokenization to automatic semicolon insertion.
    Whitespace,
    // Various operators.
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// %
    Percent,
    /// &
    Ampersand,
    /// |
    Pipe,
    /// ^
    Caret,
    /// <<
    Lshift,
    /// >>
    Rshift,
    /// &^
    BitClear,
    // Compound operators.
    /// +=
    PlusEquals,
    /// -=
    MinusEquals,
    /// *=
    TimesEquals,
    /// /=
    DivideEquals,
    /// %=
    ModuloEquals,
    /// &=
    AndEquals,
    /// |=
    PipeEquals,
    /// ^=
    XorEquals,
    /// <<=
    LshiftEquals,
    /// >>=
    RshiftEquals,
    /// &^=
    BitClearEquals,
    /// &&
    BoolAnd,
    /// ||
    BoolOr,
    /// <-
    ChanReceive,
    /// ++
    Increment,
    /// --
    Decrement,
    /// ==
    Equals,
    /// <
    LessThan,
    /// >
    GreaterThan,
    /// =
    Assign,
    /// !
    Not,
    /// !=
    NotEqual,
    /// <=
    LessThanOrEqual,
    /// >=
    GreaterThanOrEqual,
    /// :=
    ColonAssign,
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
