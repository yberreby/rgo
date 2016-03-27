extern crate convenience;
use convenience::read_file;
use std::env;

fn main() {
    // 0th arg is the program path.
    let src_file = env::args().nth(1).unwrap();
    println!("Source file: {}", src_file);

    let s = read_file(&src_file).expect("failed to read file");
    // println!("Source file contents:\n{}", s);
    let ast = parse(&s);
}

pub fn run_tests() {
    // parse(

}

pub fn parse(s: &str) -> Ast {
    unimplemented!()
}
