extern crate rgo;
extern crate convenience as cnv;
extern crate env_logger;

use std::fs;
use std::path::{Path, PathBuf};
use std::env;

fn test_path<P: AsRef<Path>>(path: P) -> PathBuf {
    let mut new = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    new.push("tests");
    new.push("data");
    new.push(path);
    new
}

fn for_all_in<P: AsRef<Path>, F: Fn(String) -> U, U>(path: P, f: F) {
    let entries = fs::read_dir(test_path(path)).unwrap();
    for entry in entries {
        let path = entry.unwrap().path();
        let src = cnv::read_file(&path).unwrap();
        println!("processing {}", path.display());
        f(src);
    }
}

fn main() {
    env::set_var("RUST_LOG", "trace");
    env::set_var("RUST_BACKTRACE", "1");
    env_logger::init().unwrap();

    lexing_does_not_panic();
    parsing_does_not_panic();
}

fn lexing_does_not_panic() {
    for_all_in("pass", |src| {
        rgo::lexer::tokenize(&src);
    });
}

fn parsing_does_not_panic() {
    for_all_in("pass", |src| {
        let tokens: Vec<_> = rgo::lexer::Lexer::new(&src).collect();
        match rgo::Parser::new(tokens.clone().into_iter()).parse() {
            Ok(_) => {}
            Err(e) => {
                // println!("Tokens:\n{:?}", tokens);
                panic!("{:?}", e);
            }
        }
    });
}
