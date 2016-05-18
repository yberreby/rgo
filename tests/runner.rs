#[macro_use]
extern crate log;
extern crate rgo;
extern crate convenience as cnv;
extern crate env_logger;
extern crate colored;

use colored::*;

use std::fs;
use std::path::{Path, PathBuf};
use std::env;

fn flush() {
    use std::io::Write;

    ::std::io::stdout().flush().unwrap();
}

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
        debug!("processing {}", path.display());
        f(src);
    }
}

fn main() {
    env::set_var("RUST_LOG", env::var("LOG").unwrap_or("info".into()));
    env::set_var("RUST_BACKTRACE", "1");
    env_logger::init().unwrap();

    lexing_does_not_panic();
    parsing_does_not_panic();
}

fn lexing_does_not_panic() {
    print!("making sure lexing doesn't panic... ");
    flush();

    for_all_in("pass", |src| {
        rgo::lexer::tokenize(&src);
    });
    println!("{}", "OK.".green());
}

fn parsing_does_not_panic() {
    print!("making sure parsing doesn't panic... ");
    flush();

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

    println!("{}", "OK.".green());
}
