extern crate convenience;
extern crate rgo;
use convenience::read_file;
use std::env;

fn main() {
    // 0th arg is the program path.
    let src_file = env::args().nth(1).unwrap();
    println!("Source file: {}", src_file);

    let s = read_file(&src_file).expect("failed to read file");
    let tokens = rgo::lexer::tokenize(&s);
    println!("Token stream:\n{:?}", tokens);
}
