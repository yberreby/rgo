//! Welcome to `rgo`!
//!
//! Compilation is a pipeline, which works (roughly) like this:
//!
//! Lexical analysis -> Parsing -> Translation
//!
//! Each part should be loosely coupled with the others, only relying on a somewhat stable public
//! API.

pub mod lexer;
