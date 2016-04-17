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
}

impl ErrorKind {
    pub fn unexpected_token(expected: Vec<TokenKind>, found: Token) -> ErrorKind {
        ErrorKind::UnexpectedToken {
            found: found,
            expected: expected,
        }
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
        }
    }
}

// quick_error! {
//     #[derive(Debug)]
//     pub enum ParseError {
//         UnexpectedToken {
//             found: Token,
//             expected: Vec<TokenKind>
//         } {
//             description("encountered an unexpected token")
//             display(me) -> ("expected one of {:?}, found {}", expected, token)
//         }
//     }
// }
//
// quick_error! {
//     #[derive(Debug)]
//     pub enum IoWrapper {
//         Io(err: io::Error) {
//             from()
//             description("io error")
//             display("I/O error: {}", err)
//             cause(err)
//         }
//         Other(descr: &'static str) {
//             description(descr)
//             display("Error {}", descr)
//         }
//         IoAt { place: &'static str, err: io::Error } {
//             cause(err)
//             display(me) -> ("{} {}: {}", me.description(), place, err)
//             description("io error at")
//             from(s: String) -> {
//                 place: "some string",
//                 err: io::Error::new(io::ErrorKind::Other, s)
//             }
//         }
//         Discard {
//             from(&'static str)
//         }
//     }
// }
