#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

#[macro_use]
extern crate log;
// XXX: do we still need this?
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate quick_error;

extern crate num;

mod pos;
pub use self::pos::Position;

pub mod token;
pub mod ast;
pub mod lexer;
pub mod parser;

pub use parser::Parser;

pub fn parse(src: &str) -> ast::SourceFile {
    let lexer = lexer::Lexer::new(src).collect();
    parser::parse_tokens(lexer)
}
