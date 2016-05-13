use super::{Token, TokenKind, tokenize};
use token::TokenKind::*;

fn assert_tokens(code: &str, expect: &[(TokenKind, Option<&str>)]) {
    let got = tokenize(code);

    assert_eq!(got.len(), expect.len());
    for (got_t, expect_t) in got.iter().zip(expect) {
        let nt = Token {
            kind: expect_t.0,
            value: expect_t.1.map(|s| s.to_owned()),
        };
        assert_eq!(got_t.token, nt);
    }
}

fn assert_token(code: &str, expect_kind: TokenKind, expect_value: Option<&str>) {
    assert_tokens(code, &[(expect_kind, expect_value)]);
}

#[test]
fn test_numerical_tokens() {
    // Integer Literals
    assert_token("42", Decimal, Some("42"));
    assert_token("0600", Octal, Some("0600"));
    assert_token("0xBadFace", Hex, Some("0xBadFace"));
    assert_token("170141183460469231731687303715884105727",
                 Decimal,
                 Some("170141183460469231731687303715884105727"));

    let float_tests = ["0.",
                       "72.40",
                       "072.40",
                       "2.71828",
                       "1.e+0",
                       "6.67428e-11",
                       "1E6",
                       ".25",
                       ".12345E+5"];

    for t in &float_tests {
        assert_token(t, Float, Some(t));
    }

    let imaginary_tests = ["0i",
                           "011i",
                           "0.i",
                           "2.71828i",
                           "1.e+0i",
                           "6.67428e-11i",
                           "1E6i",
                           ".25i",
                           ".12345E+5i"];

    for t in &imaginary_tests {
        assert_token(t, Imaginary, Some(t));
    }
}

#[test]
fn test_text_literals() {
    assert_token("'a'", Rune, Some("a"));
    assert_token("'\\n'", Rune, Some("\\n"));
    assert_token("'\\''", Rune, Some("\\'"));
    assert_token("\"Hello!\"", Str, Some("Hello!"));
    assert_token("\"\\n\\n\"", Str, Some("\\n\\n"));
    assert_token("\"\\\"\"", Str, Some("\\\""));
    assert_token("`Hello!`", StrRaw, Some("Hello!"));
    assert_token("`\\n\\n`", StrRaw, Some("\\n\\n"));
    assert_token("`\\\"`", StrRaw, Some("\\\""));
}

/// Test 'simple' tokens (tokens that do not contain a value).
#[test]
fn tokenize_simple() {
    let pairs = vec![("(", TokenKind::LParen),
                     (")", TokenKind::RParen),
                     ("{", TokenKind::LBrace),
                     ("}", TokenKind::RBrace),
                     ("[", TokenKind::LBracket),
                     ("]", TokenKind::RBracket),
                     (",", TokenKind::Comma),
                     (";", TokenKind::Semicolon),
                     (".", TokenKind::Dot),
                     ("...", TokenKind::Ellipsis),
                     ("|", TokenKind::Or),
                     ("||", TokenKind::OrOr),
                     ("|=", TokenKind::OrAssign),
                     ("!", TokenKind::Not),
                     ("!=", TokenKind::NotEqual),
                     ("^", TokenKind::Caret),
                     ("^=", TokenKind::CaretAssign),
                     ("%", TokenKind::Percent),
                     ("%=", TokenKind::PercentAssign),
                     ("&", TokenKind::And),
                     ("&&", TokenKind::AndAnd),
                     ("&=", TokenKind::AndAssign),
                     ("&^", TokenKind::BitClear),
                     ("&^=", TokenKind::BitClearAssign),
                     ("&", TokenKind::And),
                     ("&&", TokenKind::AndAnd),
                     ("&=", TokenKind::AndAssign),
                     ("&^", TokenKind::BitClear),
                     ("&^=", TokenKind::BitClearAssign),
                     ("+", TokenKind::Plus),
                     ("++", TokenKind::Increment),
                     ("+=", TokenKind::PlusAssign),
                     ("-", TokenKind::Minus),
                     ("--", TokenKind::Decrement),
                     ("-=", TokenKind::MinusAssign),
                     (":", TokenKind::Colon),
                     (":=", TokenKind::ColonAssign),
                     ("<", TokenKind::LessThan),
                     ("<-", TokenKind::Arrow),
                     ("<=", TokenKind::LessThanOrEqual),
                     ("<<", TokenKind::Lshift),
                     ("<<=", TokenKind::LshiftAssign),
                     (">", TokenKind::GreaterThan),
                     (">=", TokenKind::GreaterThanOrEqual),
                     (">>", TokenKind::Rshift),
                     (">>=", TokenKind::RshiftAssign),
                     ("*", TokenKind::Star),
                     ("*=", TokenKind::StarAssign),
                     ("=", TokenKind::Assign),
                     ("==", TokenKind::Equals),
                     ("/", TokenKind::Slash),
                     ("/=", TokenKind::SlashAssign),
    ];

    for (src, kind) in pairs {
        assert_token(src, kind, None);
    }
}

#[test]
fn tokenize_comments() {
    assert_tokens("// Hello, this is a comment", &[]);
    assert_tokens("foo /* this is a general comment */ := 2",
                  &[(Ident, Some("foo")), (ColonAssign, None), (Decimal, Some("2"))]);
}


#[test]
fn tokenize_ident() {
    let test_ident = |s| {
        assert_tokens("foo", &[(TokenKind::Ident, Some("foo"))]);
    };

    // XXX: add quickcheck test?
    test_ident("foo");
}

#[test]
fn tokenize_keywords() {
    let pairs = [("break", TokenKind::Break),
                 ("case", TokenKind::Case),
                 ("chan", TokenKind::Chan),
                 ("const", TokenKind::Const),
                 ("continue", TokenKind::Continue),
                 ("default", TokenKind::Default),
                 ("defer", TokenKind::Defer),
                 ("else", TokenKind::Else),
                 ("fallthrough", TokenKind::Fallthrough),
                 ("for", TokenKind::For),
                 ("func", TokenKind::Func),
                 ("go", TokenKind::Go),
                 ("goto", TokenKind::Goto),
                 ("if", TokenKind::If),
                 ("import", TokenKind::Import),
                 ("interface", TokenKind::Interface),
                 ("map", TokenKind::Map),
                 ("package", TokenKind::Package),
                 ("range", TokenKind::Range),
                 ("return", TokenKind::Return),
                 ("select", TokenKind::Select),
                 ("struct", TokenKind::Struct),
                 ("switch", TokenKind::Switch),
                 ("type", TokenKind::Type),
                 ("var", TokenKind::Var)];

    for &(s, k) in &pairs {
        assert_token(s, k, None);
    }
}

#[test]
fn tokenize_mixed_whitespace() {
    assert_tokens(" \t

                        \t  ",
                  &[]);
}

#[test]
fn tokenize_package_declaration() {
    assert_tokens("package main",
                  &[(TokenKind::Package, None), (TokenKind::Ident, Some("main"))]);
}

#[test]
fn tokenize_plain_interpreted_str() {
    assert_token("\"hello\"", TokenKind::Str, Some("hello"));
}

#[test]
fn tokenize_simple_import() {
    assert_tokens("import \"fmt\"",
                  &[(TokenKind::Import, None), (TokenKind::Str, Some("fmt"))]);
}
