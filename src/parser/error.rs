quick_error! {
    #[derive(Debug)]
    pub enum ParseError {
        UnexpectedToken {
            found: Token,
            expected: Vec<TokenKind>
        } {
            description("encountered an unexpected token")
            display(me) -> ("expected one of {:?}, found {}", expected, token)
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum IoWrapper {
        Io(err: io::Error) {
            from()
            description("io error")
            display("I/O error: {}", err)
            cause(err)
        }
        Other(descr: &'static str) {
            description(descr)
            display("Error {}", descr)
        }
        IoAt { place: &'static str, err: io::Error } {
            cause(err)
            display(me) -> ("{} {}: {}", me.description(), place, err)
            description("io error at")
            from(s: String) -> {
                place: "some string",
                err: io::Error::new(io::ErrorKind::Other, s)
            }
        }
        Discard {
            from(&'static str)
        }
    }
}
