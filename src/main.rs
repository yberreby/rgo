extern crate convenience;
extern crate rgo;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate time;
use convenience::read_file;
use std::env;
use time::PreciseTime;

fn main() {
    env_logger::init().unwrap();
    // 0th arg is the program path.
    let src_file = env::args().nth(1).unwrap();
    info!("Source file: {}", src_file);

    let s = read_file(&src_file).expect("failed to read file");
    let start = PreciseTime::now();
    let tokens = rgo::lexer::tokenize(&s);
    println!("Lexing: {} µs",
             start.to(PreciseTime::now()).num_microseconds().unwrap());

    let ast: rgo::ast::SourceFile = rgo::parser::parse_tokens(tokens);
    println!("AST:\n{:?}", ast);
}
