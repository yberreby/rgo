use super::*;

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
    use super::TokenKind::Literal;
    use super::Literal::*;

    // Integer Literals
    assert_token("42", Literal(Decimal), Some("42"));
    assert_token("0600", Literal(Octal), Some("0600"));
    assert_token("0xBadFace", Literal(Hex), Some("0xBadFace"));
    assert_token("170141183460469231731687303715884105727",
                 Literal(Decimal),
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
        assert_token(t, Literal(Float), Some(t));
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
        assert_token(t, Literal(Imaginary), Some(t));
    }
}
