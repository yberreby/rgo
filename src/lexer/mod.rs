//! # Lexer
//!
//! A `Lexer` parses a source string into a list of tokens, which may later be used to construct an
//! Abstract Syntax Tree.
//!
//! ## Notes
//!
//! - We want meaningful errors from the start. That means printing the line and column number on
//! error, returning `Result`s instead of panicking (later on, we may use unwinding to speed up
//! lexical analysis in non-erroneous cases).
//!
//! - It is unclear whether we should operator on Unicode `char`, or plain bytes `u8`. `char`s are
//! more convenient to display and offer a clean API; bytes are (most likely) faster to work with.
//!
//! - I'm not sure what the best way to store tokens is. A slice into the original source, an
//! interned string...? Probably an interned string, this is what rustc uses and it speeds up
//! comparisons, which are going to be very frequent. Probably reduces allocations, too - and we're
//! allocating a _lot_. We'd have to benchmark to be sure.

use std::iter::Iterator;
pub use token::*;

#[cfg(test)]
mod test;

pub struct Lexer<'src> {
    /// Byte offset from the start of the source string.
    offset: usize,
    /// The source string.
    src: &'src str,
    /// The last character to be read.
    current_char: Option<char>,
    /// The kind of token we read last. Used for automatic semicolon insertion.
    last_token_kind: Option<TokenKind>,
}

impl<'src> Lexer<'src> {
    /// Create a new Lexer from the given source string.
    pub fn new(s: &str) -> Lexer {
        // Initialize the lexer with the first character of the source string.
        let first_char = s.chars().next();

        Lexer {
            src: s,
            offset: 0,
            current_char: first_char,
            last_token_kind: None,
        }
    }

    /// 'eat' one character.
    /// This is a _very_ hot function.
    fn bump(&mut self) {
        self.offset += self.current_char.unwrap().len_utf8();

        if self.offset < self.src.len() {
            let ch = char_at(&self.src, self.offset);
            self.current_char = Some(ch);
        } else {
            self.current_char = None;
        }
    }

    /// Return the next character **without** bumping.
    /// Useful for lookahead.
    fn next_char(&self) -> Option<char> {
        let next_offset = self.offset + 1;
        if next_offset < self.src.len() {
            let ch = char_at(&self.src, next_offset);
            Some(ch)
        } else {
            None
        }
    }

    /// Scan a number literal (integer or float).
    ///
    /// The literal "0" is considered octal, [as in
    /// C++](http://stackoverflow.com/a/6895543/2754323).
    fn scan_number(&mut self) -> Token {
        // Integer literal grammar:
        //
        // int_lit     = decimal_lit | octal_lit | hex_lit .
        // decimal_lit = ( "1" … "9" ) { decimal_digit } .
        // octal_lit   = "0" { octal_digit } .
        // hex_lit     = "0" ( "x" | "X" ) hex_digit { hex_digit } .

        let start = self.offset;

        // If we have a hexadecimal, treat it specially.
        if self.current_char == Some('0') &&
           (self.next_char() == Some('x') || self.next_char() == Some('x')) {
            self.bump();
            self.bump();

            while let Some(c) = self.current_char {
                if c.is_digit(16) {
                    self.bump();
                } else {
                    break;
                }
            }

            return Token {
                value: Some(self.src[start..self.offset].into()),
                kind: TokenKind::Hex,
            };
        }

        let has_leading_zero = self.current_char == Some('0');
        let mut had_e = false;
        let mut had_dot = false;

        'outer: while let Some(c) = self.current_char {
            if c.is_digit(10) {
                self.bump();
            } else if !had_e && (c == 'e' || c == 'E') {
                self.bump();
                had_e = true;

                if self.current_char == Some('+') || self.current_char == Some('-') {
                    self.bump();
                }
            } else if !had_e && !had_dot && c == '.' {
                self.bump();
                had_dot = true;
            } else if c == 'i' {
                self.bump();

                return Token {
                    value: Some(self.src[start..self.offset].into()),
                    kind: TokenKind::Imaginary,
                };
            } else {
                break;
            }
        }

        let s = &self.src[start..self.offset];

        let kind = if had_e || had_dot {
            TokenKind::Float
        } else if has_leading_zero {
            TokenKind::Octal
        } else {
            TokenKind::Decimal
        };

        Token {
            value: Some(s.into()),
            kind: kind,
        }
    }

    /// Skip whitespace and comments, returning whether at least one newline was encountered.
    fn skip_whitespace_and_comments(&mut self) -> bool {
        let mut contains_newline = false;

        while let Some(c) = self.current_char {
            if c == '\n' {
                contains_newline = true;
            }

            // Are we at the start of a general comment (`/* ... */`)?
            if c == '/' && self.next_char() == Some('*') {
                // Skip the '/*'.
                self.bump();
                self.bump();

                // Skip the comment body.
                while let Some(c) = self.current_char {
                    if c == '*' && self.next_char() == Some('/') {
                        break;
                    } else {
                        self.bump();
                    }
                }

                // Skip the '*/'.
                self.bump();
                self.bump();

                // Resume whitespace skipping.
                continue;

            } else if c == '/' && self.next_char() == Some('/') {
                while let Some(c) = self.current_char {
                    if c == '\n' {
                        break;
                    } else {
                        self.bump();
                    }
                }

                // Resume whitespace skipping.
                // Since we have not bumped past the newline character,
                // the next iteration of the loop will catch it.
                continue;
            }

            if c.is_whitespace() {
                self.bump();
            } else {
                break;
            }
        }


        contains_newline
    }

    fn scan_ident(&mut self) -> &str {
        let start = self.offset;

        while let Some(c) = self.current_char {
            if can_continue_identifier(c) {
                self.bump();
            } else {
                break;
            }
        }

        &self.src[start..self.offset]
    }

    fn scan_ident_or_keyword(&mut self) -> Token {
        let ident = self.scan_ident();
        let mut value = None;

        use token::TokenKind::*;

        let kind = match &*ident {
            "break" => Break,
            "case" => Case,
            "chan" => Chan,
            "const" => Const,
            "continue" => Continue,
            "default" => Default,
            "defer" => Defer,
            "else" => Else,
            "fallthrough" => Fallthrough,
            "for" => For,
            "func" => Func,
            "go" => Go,
            "goto" => Goto,
            "if" => If,
            "import" => Import,
            "interface" => Interface,
            "map" => Map,
            "package" => Package,
            "range" => Range,
            "return" => Return,
            "select" => Select,
            "struct" => Struct,
            "switch" => Switch,
            "type" => Type,
            "var" => Var,
            // XXX(perf): unnecessary alloc.
            _ => {
                value = Some(ident.into());
                TokenKind::Ident
            }
        };

        Token {
            kind: kind,
            value: value,
        }
    }

    /// Return the next token, if any.
    fn next_token_inner(&mut self) -> Option<Token> {
        // Whitespace and comment handling.
        let contains_newline = self.skip_whitespace_and_comments();

        // Automatic semicolon insertion in the simplest case (newline + token that may terminate a
        // statement).
        //
        // The Go Spec also says that a semicolon may be omitted before a closing ")" or "}".
        // This case is _not_ handled by the lexer, but by the parser, as it requires too much
        // context.
        if contains_newline && may_terminate_statement(self.last_token_kind) {
            return Some(Token {
                kind: TokenKind::Semicolon,
                value: None,
            });
        }

        // Check for EOF after whitespace handling.
        let c = match self.current_char {
            Some(c) => c,
            None => return None,
        };

        // Check for simple tokens.
        let simple_kind_mappings = [('(', TokenKind::LParen),
                (')', TokenKind::RParen),
                ('{', TokenKind::LBrace),
                ('}', TokenKind::RBrace),
                ('[', TokenKind::LBracket),
                (']', TokenKind::RBracket),
                (',', TokenKind::Comma),
                (';', TokenKind::Semicolon),
        ];

        if let Some(kind_mapping) = simple_kind_mappings.iter().find(|x| x.0 == c) {
            self.bump();
            return Some(Token {
                kind: kind_mapping.1,
                value: None,
            });
        }

        let kind = match c {
            // More complex tokens.
            '.' => {
                if self.next_char().map(|x| x.is_digit(10)) == Some(true) {
                    return Some(self.scan_number());
                }

                self.bump();

                // Look for an ellipsis ('...').
                if self.current_char == Some('.') && self.next_char() == Some('.') {
                    self.bump();
                    self.bump();
                    TokenKind::Ellipsis
                } else {
                    TokenKind::Dot
                }
            }
            ':' => {
                self.bump();

                if self.current_char == Some('=') {
                    self.bump();
                    TokenKind::ColonAssign
                } else {
                    TokenKind::Colon
                }
            }
            '=' => {
                self.bump();

                if self.current_char == Some('=') {
                    self.bump();
                    TokenKind::Equals
                } else {
                    TokenKind::Assign
                }
            }
            '+' => {
                self.bump();

                match self.current_char {
                    Some('+') => {
                        self.bump();
                        TokenKind::Increment
                    }
                    Some('=') => {
                        self.bump();
                        TokenKind::PlusAssign
                    }
                    _ => TokenKind::Plus,
                }
            }
            '-' => {
                self.bump();

                match self.current_char {
                    Some('-') => {
                        self.bump();
                        TokenKind::Decrement
                    }
                    Some('=') => {
                        self.bump();
                        TokenKind::MinusAssign
                    }
                    _ => TokenKind::Minus,
                }
            }
            '*' => {
                self.bump();

                match self.current_char {
                    Some('=') => {
                        self.bump();
                        TokenKind::StarAssign
                    }
                    _ => TokenKind::Star,
                }
            }
            '/' => {
                self.bump();

                match self.current_char {
                    Some('=') => {
                        self.bump();
                        TokenKind::SlashAssign
                    }
                    _ => TokenKind::Slash,
                }
            }
            '<' => {
                self.bump();

                match self.current_char {
                    Some('<') => {
                        self.bump();
                        match self.current_char {
                            Some('=') => {
                                self.bump();
                                TokenKind::LshiftAssign
                            }
                            _ => TokenKind::Lshift,
                        }
                    }
                    Some('=') => {
                        self.bump();
                        TokenKind::LessThanOrEqual
                    }
                    Some('-') => {
                        self.bump();
                        TokenKind::Arrow
                    }
                    _ => TokenKind::LessThan,
                }
            }
            '>' => {
                self.bump();

                match self.current_char {
                    Some('>') => {
                        self.bump();
                        match self.current_char {
                            Some('=') => {
                                self.bump();
                                TokenKind::RshiftAssign
                            }
                            _ => TokenKind::Rshift,
                        }
                    }
                    Some('=') => {
                        self.bump();
                        TokenKind::GreaterThanOrEqual
                    }
                    _ => TokenKind::GreaterThan,
                }
            }
            '|' => {
                self.bump();

                match self.current_char {
                    Some('|') => {
                        self.bump();
                        TokenKind::OrOr
                    }
                    Some('=') => {
                        self.bump();
                        TokenKind::OrAssign
                    }
                    _ => TokenKind::Or,
                }
            }
            '&' => {
                self.bump();

                match self.current_char {
                    Some('&') => {
                        self.bump();
                        TokenKind::AndAnd
                    }
                    Some('=') => {
                        self.bump();
                        TokenKind::AndAssign
                    }
                    Some('^') => {
                        self.bump();
                        match self.current_char {
                            Some('=') => {
                                self.bump();
                                TokenKind::BitClearAssign
                            }
                            _ => TokenKind::BitClear,
                        }
                    }
                    _ => TokenKind::And,
                }
            }
            '!' => {
                self.bump();

                match self.current_char {
                    Some('=') => {
                        self.bump();
                        TokenKind::NotEqual
                    }
                    _ => TokenKind::Not,
                }
            }
            '^' => {
                self.bump();

                match self.current_char {
                    Some('=') => {
                        self.bump();
                        TokenKind::CaretAssign
                    }
                    _ => TokenKind::Caret,
                }
            }
            '%' => {
                self.bump();

                match self.current_char {
                    Some('=') => {
                        self.bump();
                        TokenKind::PercentAssign
                    }
                    _ => TokenKind::Percent,
                }
            }
            // Scan integer.
            c if c.is_digit(10) => return Some(self.scan_number()),
            c if can_start_identifier(c) => return Some(self.scan_ident_or_keyword()),
            // Start of _interpreted_ string literal.
            '"' => return Some(self.scan_interpreted_str_lit()),
            '`' => return Some(self.scan_raw_str_lit()),
            '\'' => return Some(self.scan_rune_lit()),
            c => panic!("unexpected start of token: '{}'", c),
        };

        Some(Token {
            kind: kind,
            value: None,
        })
    }

    fn scan_rune_lit(&mut self) -> Token {
        self.bump();

        let start = self.offset;

        while let Some(c) = self.current_char {
            // If we encounter a backslash escape, we just skip past the '\' and the
            // following character.
            if c == '\\' {
                self.bump();
                self.bump();
            } else if c == '\'' {
                break;
            } else {
                self.bump();
            }
        }

        let s = &self.src[start..self.offset];

        // Skip the quote _after_ slicing so that it isn't included
        // in the slice.
        self.bump();

        Token {
            value: Some(s.into()),
            kind: TokenKind::Rune,
        }
    }

    fn scan_interpreted_str_lit(&mut self) -> Token {
        self.bump();
        let start = self.offset;

        while let Some(c) = self.current_char {
            // If we encounter a backslash escape, we just skip past the '\' and the
            // following character.
            if c == '\\' {
                self.bump();
                self.bump();
            } else if c == '"' {
                break;
            } else {
                self.bump();
            }
        }

        let s = &self.src[start..self.offset];

        // Skip the quote _after_ slicing so that it isn't included
        // in the slice.
        self.bump();

        Token {
            // XXX(perf): alloc.
            value: Some(s.into()),
            kind: TokenKind::Str,
        }
    }

    fn scan_raw_str_lit(&mut self) -> Token {
        // Bump past the opening backtrick.
        self.bump();
        let start = self.offset;

        while let Some(c) = self.current_char {
            // Raw strings are pretty simple, because we don't have to handle escapes.
            if c == '`' {
                break;
            } else {
                self.bump();
            }
        }

        let s = &self.src[start..self.offset];

        // Skip the backtick _after_ slicing so that it isn't included
        // in the slice.
        self.bump();

        Token {
            // XXX(perf): alloc.
            value: Some(s.into()),
            kind: TokenKind::StrRaw,
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = TokenAndSpan;

    fn next(&mut self) -> Option<TokenAndSpan> {
        let start = self.offset as u32;
        let t = self.next_token_inner();
        self.last_token_kind = t.as_ref().map(|t| t.kind);

        t.map(|t| {
            TokenAndSpan {
                token: t,
                span: Span {
                    start: start,
                    end: self.offset as u32,
                },
            }
        })
    }
}

/// Convenience function to collect all the tokens from a string.
pub fn tokenize(s: &str) -> Vec<TokenAndSpan> {
    let lexer = Lexer::new(s);
    lexer.collect()
}


// =====
// Utility functions.
//
// XXX(perf): expensive checks on Unicode chars (is_alphabetic(), is_numeric()) in these functions.
// =====

fn can_start_identifier(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn can_continue_identifier(c: char) -> bool {
    c.is_alphabetic() || c.is_numeric() || c == '_'
}

fn char_at(s: &str, byte: usize) -> char {
    s[byte..].chars().next().unwrap()
}

// For automatic semicolon insertion.
fn may_terminate_statement(t: Option<TokenKind>) -> bool {
    // A non-existent token may not terminate a line.
    let t = match t {
        Some(t) => t,
        None => return false,
    };

    // From the Go spec:
    //
    // When the input is broken into tokens, a semicolon is automatically inserted into the
    // token stream immediately after a line's final token if that token is:
    // - an identifier
    // - an integer, floating-point, imaginary, rune, or string literal
    // - one of the keywords break, continue, fallthrough, or return
    // - one of the operators and delimiters ++, --, ), ], or }
    use token::TokenKind::*;
    match t {
        Ident => true,
        Increment | Decrement | Break | Continue | Fallthrough | Return | RParen | RBracket |
        RBrace => true,
        t if t.is_literal() => true,
        _ => false,
    }
}
