//! # Lexer
//!
//! A `Lexer` parses a source string into a list of tokens, which may later be used to construct an
//! Abstract Syntax Tree.
//!
//! ## Notes
//!
//! We want meaningful errors from the start. That means printing the line and column number on
//! error, returning `Result`s instead of panicking (later on, we may use unwinding to speed up
//! lexical analysis in non-erroneous cases).
//!
//! It is unclear whether we should operator on Unicode `char`, or plain bytes `u8`. `char`s are
//! more convenient to display; bytes are (most likely) faster to work with.

use std::iter::Iterator;

mod token;
pub use self::token::*;

#[cfg(test)]
mod test;

pub struct Lexer<'src> {
    /// Byte offset from the start.
    pos: usize,
    /// The source string.
    src: &'src str,
    // XXX: char or u8?
    /// The last byte that was read.
    current_byte: Option<u8>,
}

impl<'src> Lexer<'src> {
    pub fn new(s: &str) -> Lexer {
        let first_byte = s.as_bytes().get(0).cloned();
        println!("first_byte: {:?}", first_byte);
        let mut l = Lexer {
            src: s,
            pos: 0,
            current_byte: first_byte, // Ugly?
        };

        // l.bump();
        l
    }

    /// 'eat' one character.
    fn bump(&mut self) {
        let old = self.current_byte;
        self.pos += 1;
        self.current_byte = self.src.as_bytes().get(self.pos).cloned();
        // XXX: calling as_bytes every time - perf impact?
        // This is an Option<u8>.
        // xxx(perf): .clone() or .map(|&b| b) or .map(|b| *b)?
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;

    /// Return the next token, if any.
    ///
    /// A fundamental property of this function is that **the next token does not depend on the
    /// previous one**.
    /// This means many syntactically incorrect inputs, such as `, , ,` or `;+m/^`, can pass
    /// tokenization, even though they would fail parsing.
    /// This also means testing whether a single token is tokenized properly does not require
    /// scaffolding (i.e. building an entire test program), which is a good thing.
    ///
    /// # Example
    ///
    /// ```
    /// use rgo::lexer::{Lexer, Token, DelimToken};
    ///
    /// let mut lexer = Lexer::new(")");
    /// assert_eq!(lexer.next(), Some(Token::CloseDelim(DelimToken::Paren)));
    /// ```
    fn next(&mut self) -> Option<Token> {
        // Stop tokenizing on EOF.
        let c = match self.current_byte {
            Some(c) => c,
            None => return None,
        };

        match c {
            // Single-character tokens.
            b'(' => {
                self.bump();
                return Some(Token::OpenDelim(DelimToken::Paren));
            }
            b')' => {
                self.bump();
                return Some(Token::CloseDelim(DelimToken::Paren));
            }
            b'{' => {
                self.bump();
                return Some(Token::OpenDelim(DelimToken::Brace));
            }
            b'}' => {
                self.bump();
                return Some(Token::CloseDelim(DelimToken::Brace));
            }
            b'[' => {
                self.bump();
                return Some(Token::OpenDelim(DelimToken::Bracket));
            }
            b']' => {
                self.bump();
                return Some(Token::CloseDelim(DelimToken::Bracket));
            }

            _ => panic!("unexpected char"),
        }

        unimplemented!()
    }
}

/// Convenience function to collect all the tokens from a string.
///
/// # Example
///
/// ```
/// use rgo::lexer::{tokenize, Token, DelimToken};
///
/// assert_eq!(tokenize("()"), vec![
///     Token::OpenDelim(DelimToken::Paren),
///     Token::CloseDelim(DelimToken::Paren)
/// ]);
/// ```
pub fn tokenize(s: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(s);
    let tokens: Vec<Token> = lexer.collect();

    tokens
}
