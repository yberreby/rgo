// XXX: whitespace shall be a token.

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
    Dot,
    /// ;
    Semicolon,
    /// :
    Colon,
}


//  An integer literal is a sequence of digits representing an integer constant. An optional prefix
//  sets a non-decimal base: 0 for octal, 0x or 0X for hexadecimal. In hexadecimal literals, letters
//  a-f and A-F represent values 10 through 15.
//
// int_lit     = decimal_lit | octal_lit | hex_lit .
// decimal_lit = ( "1" … "9" ) { decimal_digit } .
// octal_lit   = "0" { octal_digit } .
// hex_lit     = "0" ( "x" | "X" ) hex_digit { hex_digit } .
//
// 42
// 0600
// 0xBadFace
// 170141183460469231731687303715884105727

pub enum Literal {
    Integer(String),
    Float(String),
    Imaginary(String),
    Rune(String),
    /// Interpreted string literal.
    Str(String),
    /// Raw string literal.
    StrRaw(String),
}

// XXX: stored as strings. Consider interning strings.

pub enum DelimToken {
    /// A round parenthesis: `(` or `)`
    Paren,
    /// A square bracket: `[` or `]`
    Bracket,
    /// A curly brace: `{` or `}`
    Brace,
}
