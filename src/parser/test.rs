use super::*;
use lexer;

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
                        ("\\xe6\\x97\\xa5\\xe6\\x9c\\xac\\xe8\\xaa\\x9e", "日本語")];

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
