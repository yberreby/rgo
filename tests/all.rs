extern crate rgo;
extern crate convenience as cnv;

use std::fs;
use std::path::{Path, PathBuf};

fn test_path<P: AsRef<Path>>(path: P) -> PathBuf {
    let mut new = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    new.push("test-data");
    new.push(path);
    new
}

fn for_all_in<P: AsRef<Path>, F: Fn(String) -> U, U>(path: P, f: F) {
    let entries = fs::read_dir(test_path(path)).unwrap();

    for entry in entries {
        let path = entry.unwrap().path();
        let src = cnv::read_file(path).unwrap();
        f(src);
    }
}

#[test]
fn lexing_does_not_panic() {
    for_all_in("pass", |src| {
        rgo::lexer::tokenize(&src);
    });
}

#[test]
fn parsing_does_not_panic() {
    for_all_in("pass", |src| {
        rgo::parse(&src);
    });
}
