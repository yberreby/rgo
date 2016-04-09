//! Welcome to `rgo`!
//!
//! Compilation is a pipeline, which works (roughly) like this:
//!
//! Lexical analysis -> Parsing -> Translation
//!
//! Each part should be loosely coupled with the others, only relying on a somewhat stable public
//! API.

#[macro_use]
extern crate log;

pub mod ast;
pub mod lexer;
pub mod parser;

pub use parser::Parser;
