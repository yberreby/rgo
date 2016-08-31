#![feature(test)]


/// Utility macro to bench test files one by one.
macro_rules! bench_lex {
    ($name:ident, $path:expr) => {
        #[allow(non_snake_case)]
        mod $name {
            extern crate convenience;
            extern crate test;
            extern crate rgo;

            use self::test::Bencher;
            use std::path::Path;
            use std::path::PathBuf;

            fn test_path<P: AsRef<Path>>(path: P) -> PathBuf {
                let mut new = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
                new.push("tests");
                new.push("data");
                new.push(path.as_ref());
                new
            }

            #[bench]
            fn bench_lex(b: &mut Bencher) {
                let src = convenience::read_file(test_path($path)).unwrap();
                b.bytes = src.len() as u64;

                b.iter(|| {
                    rgo::lexer::tokenize(&src);
                });
            }
        }
    }
}

bench_lex!(viper, "pass/viper.go");
bench_lex!(hello, "pass/hello.go");
bench_lex!(arithConst_ssa, "pass/arithConst_ssa.go");
bench_lex!(rewriteAMD64, "pass/rewriteAMD64.go");
