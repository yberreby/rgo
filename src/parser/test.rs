use super::*;
use std::str::FromStr;
use lexer;
use ast;
use num::bigint::BigInt;
use num::BigRational;

// String literals

fn assert_interpret_string_eq(lit: &str, expect: Vec<u8>) {
    let tokens = lexer::tokenize(format!("\"{}\"", lit).as_ref());

    assert_eq!(tokens.len(), 1);

    let mut p = Parser::new(tokens.into_iter());

    let got = p.interpret_string_lit(lit.to_owned()).unwrap();

    assert_eq!(expect, got);
}

fn assert_interpret_string_valid(lit: &str) {
    let tokens = lexer::tokenize(format!("\"{}\"", lit).as_ref());

    assert_eq!(tokens.len(), 1);

    let mut p = Parser::new(tokens.into_iter());

    p.interpret_string_lit(lit.to_owned()).unwrap();
}

#[test]
fn test_interpret_strings() {
    let string_tests = [("hello", "hello"),
                        ("newline \\n", "newline \n"),
                        ("\\\"", "\""),
                        ("日本語", "日本語"),
                        ("\\u65e5本\\U00008a9e", "日本語"),
                        ("\\u65e5\\u672c\\u8a9e", "日本語"),
                        ("\\U000065e5\\U0000672c\\U00008a9e", "日本語"),
                        ("\\xe6\\x97\\xa5\\xe6\\x9c\\xac\\xe8\\xaa\\x9e", "日本語"),
                        ("\\150", "h")];

    for t in &string_tests {
        assert_interpret_string_eq(t.0, t.1.into());
    }

    assert_interpret_string_eq("\\xff\\u00FF", vec![255, 195, 191]);
}

#[test]
#[should_panic]
fn test_interpret_string_illegal_surrogate_half() {
    assert_interpret_string_valid("\\uD800");
}

#[test]
#[should_panic]
fn test_interpret_string_invalid_codepoint() {
    assert_interpret_string_valid("\\U00110000");
}

// Rune literals

fn assert_interpret_rune_eq(lit: &str, expect: char) {
    let tokens = lexer::tokenize(format!("'{}'", lit).as_ref());

    assert_eq!(tokens.len(), 1);

    let mut p = Parser::new(tokens.into_iter());

    let got = p.parse_rune_lit().unwrap();

    assert_eq!(expect, got);
}

fn assert_interpret_rune_valid(lit: &str) {
    let tokens = lexer::tokenize(format!("'{}'", lit).as_ref());

    assert_eq!(tokens.len(), 1);

    let mut p = Parser::new(tokens.into_iter());

    p.parse_rune_lit().unwrap();
}

#[test]
fn test_interpret_runes() {
    let rune_tests = [("a", 'a'),
                      ("ä", 'ä'),
                      ("本", '本'),
                      ("\\t", '\t'),
                      ("\\000", '\0'),
                      ("\\007", '\x07'),
                      ("\\377", '\u{00ff}'),
                      ("\\x07", '\x07'),
                      ("\\xff", '\u{00ff}'),
                      ("\\u12e4", '\u{12e4}'),
                      ("\\U00101234", '\u{101234}'),
                      ("\\'", '\'')];

    for &(lit, expect) in &rune_tests {
        assert_interpret_rune_eq(lit, expect);
    }
}

#[test]
#[should_panic]
fn test_interpret_rune_too_many_characters() {
    assert_interpret_rune_valid("aa");
}

#[test]
#[should_panic]
fn test_interpret_rune_too_few_hex_digits() {
    assert_interpret_rune_valid("\\xa");
}

#[test]
#[should_panic]
fn test_interpret_rune_too_few_octal_digits() {
    assert_interpret_rune_valid("\\0");
}

#[test]
#[should_panic]
fn test_interpret_rune_surrogate_half() {
    assert_interpret_rune_valid("\\uDFFF");
}

#[test]
#[should_panic]
fn test_interpret_rune_invalid_codepoint() {
    assert_interpret_rune_valid("\\u00110000");
}

// Int literals

fn assert_interpret_int_eq(lit: &str, expect: BigInt) {
    let tokens = lexer::tokenize(lit);

    assert_eq!(tokens.len(), 1);

    let mut p = Parser::new(tokens.into_iter());

    let got = p.parse_int_lit().unwrap();

    assert_eq!(expect, got);
}

#[test]
fn test_interpret_ints() {
    assert_interpret_int_eq("42", BigInt::from(42));
    assert_interpret_int_eq("0600", BigInt::from(0o600));
    assert_interpret_int_eq("0xBadFace", BigInt::from(0xbadface));
    assert_interpret_int_eq("170141183460469231731687303715884105727",
                            BigInt::from_str("170141183460469231731687303715884105727").unwrap());
}

// Float/imaginary literals

fn assert_interpret_float_eq(lit: &str, expect: BigRational) {
    let tokens = lexer::tokenize(format!("{}", lit).as_ref());

    assert_eq!(tokens.len(), 1);

    let mut p = Parser::new(tokens.into_iter());

    let got = p.parse_basic_lit().unwrap();

    if let ast::BasicLit::Float(val) = got {
        assert_eq!(expect, val);
    } else {
        panic!(format!("expected float basic lit, found {:?}", got));
    }
}

fn assert_interpret_imaginary_eq(lit: &str, expect: BigRational) {
    let tokens = lexer::tokenize(lit);

    assert_eq!(tokens.len(), 1);

    let mut p = Parser::new(tokens.into_iter());

    let got = p.parse_basic_lit().unwrap();

    if let ast::BasicLit::Imaginary(val) = got {
        assert_eq!(expect, val);
    } else {
        panic!(format!("expected imaginary basic lit, found {:?}", got));
    }
}

fn bigrat_from_int(value: u64) -> BigRational {
    BigRational::from_integer(BigInt::from(value))
}

fn bigrat_from_ints(numerator: u64, denominator: u64) -> BigRational {
    BigRational::from_integer(BigInt::from(numerator)) /
    BigRational::from_integer(BigInt::from(denominator))
}

#[test]
fn test_interpret_floats() {
    let float_tests = [("13e0", bigrat_from_int(13)),
                       ("13e4", bigrat_from_int(130000)),
                       ("13E4", bigrat_from_int(130000)),
                       ("13e+4", bigrat_from_int(130000)),
                       ("130000e-4", bigrat_from_int(13)),
                       ("1.5", bigrat_from_ints(3, 2)),

                       ("0.", bigrat_from_int(0)),
                       ("72.40", bigrat_from_ints(724, 10)),
                       ("072.40", bigrat_from_ints(724, 10)),
                       ("2.71828", bigrat_from_ints(271828, 100000)),
                       ("1.e+0", bigrat_from_int(1)),
                       ("6.67428e-11", bigrat_from_ints(667428, 10000000000000000)),
                       ("1E6", bigrat_from_int(1000000)),
                       (".25", bigrat_from_ints(25, 100)),
                       (".12345E+5", bigrat_from_int(12345))];

    for t in &float_tests {
        assert_interpret_float_eq(t.0, t.1.clone());
    }
}

#[test]
fn test_interpret_imaginaries() {
    let float_tests = [("0i", bigrat_from_int(0)),
                       ("011i", bigrat_from_int(11)),
                       ("0.i", bigrat_from_int(0)),
                       ("2.71828i", bigrat_from_ints(271828, 100000)),
                       ("1.e+0i", bigrat_from_int(1)),
                       ("6.67428e-11i", bigrat_from_ints(667428, 10000000000000000)),
                       ("1E6i", bigrat_from_int(1000000)),
                       (".25i", bigrat_from_ints(25, 100)),
                       (".12345E+5i", bigrat_from_int(12345))];

    for t in &float_tests {
        assert_interpret_imaginary_eq(t.0, t.1.clone());
    }
}
