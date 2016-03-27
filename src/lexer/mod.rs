pub enum Token {
    Identifier(String),
    Literal(Literal),
    Keyword(Keyword),
    OperatorOrDelimiter(OperatorOrDelimiter),
}


// The following keywords are reserved and may not be used as identifiers.
//
// break        default      func         interface    select
// case         defer        go           map          struct
// chan         else         goto         package      switch
// const        fallthrough  if           range        type
// continue     for          import       return       var
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

// The following character sequences represent operators, delimiters, and other special tokens:
//
// +    &     +=    &=     &&    ==    !=    (    )
// -    |     -=    |=     ||    <     <=    [    ]
// *    ^     *=    ^=     <-    >     >=    {    }
// /    <<    /=    <<=    ++    =     :=    ,    ;
// %    >>    %=    >>=    --    !     ...   .    :
//      &^          &^=

// XXX: I have created a monster.
pub enum OperatorOrDelimiter {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Caret,
    Lshift,
    Rshift,
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
    AndEquals,
    PipeEquals,
    XorEquals,
    LshiftEquals,
    RshiftEquals,
    BitClearEquals,
    BoolAnd,
    BoolOr,
    ChanReceive,
    Increment,
    Decrement,
    Equals,
    LessThan,
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
    ShortAssign,
    /// ...
    Ellipsis,
    /// (
    Lparen,
    /// )
    Rparen,
    /// [
    LeftSquareBracket,
    /// ]
    RightSquareBracket,
    /// {
    LeftCurlyBrace,
    /// }
    RightCurlyBrace,
    /// ,
    Comma,
    /// .
    Period,
    /// ;
    Semicolon,
    /// :
    Colon,
}
