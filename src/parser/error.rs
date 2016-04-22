use std::fmt;
use token::{Token, TokenKind};

pub type PResult<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub offset: u32,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    UnexpectedToken {
        found: Token,
        expected: Vec<TokenKind>,
    },
    Other {
        msg: String,
    },
}

impl ErrorKind {
    pub fn unexpected_token(expected: Vec<TokenKind>, found: Token) -> ErrorKind {
        ErrorKind::UnexpectedToken {
            found: found,
            expected: expected,
        }
    }

    // XXX: potential code bloat due to monomorphisation, unless inlined. But we don't want to
    // inline cold functions... so it could be best to remove all generics here and
    // #[inline(never)].
    pub fn other<T: Into<String>>(msg: T) -> ErrorKind {
        ErrorKind::Other { msg: msg.into() }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::UnexpectedToken { ref found, ref expected } => {
                try!(write!(f, "expected "));

                if expected.len() > 2 {
                    try!(write!(f, "one of "));

                    let mut sep = " ";
                    for tk in expected {
                        try!(write!(f, "\"{}\"{}", tk, sep));
                        sep = ", ";
                    }
                } else {
                    try!(write!(f, "\"{}\" ", expected[0]));
                }

                write!(f, "found \"{}\"", found)
            }
            ErrorKind::Other { ref msg } => write!(f, "{}", msg),
        }
    }
}
