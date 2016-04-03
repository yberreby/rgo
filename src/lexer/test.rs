use super::{Token, DelimToken, Literal, Keyword, tokenize};

/// Utility function to compare two slices of tokens.
/// Better than `assert_eq!` because it shows the mismatch more clearly.
fn assert_eq_tok<T, U>(found_s: T, expected_s: U)
    where T: AsRef<[Token]>,
          U: AsRef<[Token]>
{
    for (i, (found, expected)) in found_s.as_ref()
                                         .iter()
                                         .zip(expected_s.as_ref().iter())
                                         .enumerate() {
        if found != expected {
            panic!("unexpected token at pos {}: expected '{:?}', found '{:?}'",
                   i,
                   expected,
                   found)
        }
    }
}

#[test]
fn tokenize_delimiters() {
    assert_eq_tok(tokenize("("), vec![Token::OpenDelim(DelimToken::Paren)]);
    assert_eq_tok(tokenize(")"), vec![Token::CloseDelim(DelimToken::Paren)]);
    assert_eq_tok(tokenize("{"), vec![Token::OpenDelim(DelimToken::Brace)]);
    assert_eq_tok(tokenize("}"), vec![Token::CloseDelim(DelimToken::Brace)]);
    assert_eq_tok(tokenize("["), vec![Token::OpenDelim(DelimToken::Bracket)]);
    assert_eq_tok(tokenize("]"), vec![Token::CloseDelim(DelimToken::Bracket)]);
}

#[test]
fn tokenize_comma() {
    assert_eq_tok(tokenize(","), vec![Token::Comma]);
}

#[test]
fn tokenize_dot_variants() {
    assert_eq_tok(tokenize("."), vec![Token::Dot]);
    assert_eq_tok(tokenize("..."), vec![Token::Ellipsis]);
}

#[test]
fn tokenize_pipe_variants() {
    assert_eq_tok(tokenize("|"), vec![Token::Pipe]);
    assert_eq_tok(tokenize("||"), vec![Token::PipePipe]);
    assert_eq_tok(tokenize("|="), vec![Token::PipeEquals]);
}

#[test]
fn tokenize_plus_variants() {
    assert_eq_tok(tokenize("+"), vec![Token::Plus]);
    assert_eq_tok(tokenize("++"), vec![Token::Increment]);
    assert_eq_tok(tokenize("+="), vec![Token::PlusEquals]);
}

#[test]
fn tokenize_minus_variants() {
    assert_eq_tok(tokenize("-"), vec![Token::Minus]);
    assert_eq_tok(tokenize("--"), vec![Token::Decrement]);
    assert_eq_tok(tokenize("-="), vec![Token::MinusEquals]);
}


#[test]
fn tokenize_colon_variants() {
    assert_eq_tok(tokenize(":"), vec![Token::Colon]);
    assert_eq_tok(tokenize(":="), vec![Token::ColonAssign]);
}

#[test]
fn tokenize_ident() {
    let test_ident = |s| {
        assert_eq_tok(tokenize(s), vec![Token::Ident(s.into())]);
    };

    test_ident("foo");
}

#[test]
fn tokenize_keywords() {
    let test_keyword = |s, k| {
        assert_eq_tok(tokenize(s), vec![Token::Keyword(k)]);
    };

    test_keyword("break", Keyword::Break);
    test_keyword("case", Keyword::Case);
    test_keyword("chan", Keyword::Chan);
    test_keyword("const", Keyword::Const);
    test_keyword("continue", Keyword::Continue);
    test_keyword("default", Keyword::Default);
    test_keyword("defer", Keyword::Defer);
    test_keyword("else", Keyword::Else);
    test_keyword("fallthrough", Keyword::Fallthrough);
    test_keyword("for", Keyword::For);
    test_keyword("func", Keyword::Func);
    test_keyword("go", Keyword::Go);
    test_keyword("goto", Keyword::Goto);
    test_keyword("if", Keyword::If);
    test_keyword("import", Keyword::Import);
    test_keyword("interface", Keyword::Interface);
    test_keyword("map", Keyword::Map);
    test_keyword("package", Keyword::Package);
    test_keyword("range", Keyword::Range);
    test_keyword("return", Keyword::Return);
    test_keyword("select", Keyword::Select);
    test_keyword("struct", Keyword::Struct);
    test_keyword("switch", Keyword::Switch);
    test_keyword("type", Keyword::Type);
    test_keyword("var", Keyword::Var);
}

#[test]
fn tokenize_mixed_whitespace() {
    assert_eq_tok(tokenize(" \t
                        
                        \t  "),
                  vec![Token::Whitespace]);
}

#[test]
fn tokenize_package_declaration() {
    assert_eq_tok(tokenize("package main"),
                  vec![Token::Keyword(Keyword::Package),
                       Token::Whitespace,
                       Token::Ident("main".into())]);
}

#[test]
fn tokenize_plain_interpreted_str() {
    assert_eq_tok(tokenize("\"hello\""),
                  vec![Token::Literal(Literal::Str("hello".into()))]);
}

#[test]
fn tokenize_simple_import() {
    assert_eq_tok(tokenize("import \"fmt\""),
                  vec![Token::Keyword(Keyword::Import),
                       Token::Whitespace,
                       Token::Literal(Literal::Str("fmt".into()))]);
}

#[test]
fn tokenize_hello() {
    let src = r#"package main

import "fmt"

func main() {
	fmt.Println("Hello, rgo")
}
"#;

    let expected = [Token::Keyword(Keyword::Package),
                    Token::Whitespace,
                    Token::Ident("main".into()),
                    Token::Whitespace,
                    Token::Keyword(Keyword::Import),
                    Token::Whitespace,
                    Token::Literal(Literal::Str("fmt".into())),
                    Token::Whitespace,
                    Token::Keyword(Keyword::Func),
                    Token::Whitespace,
                    Token::Ident("main".into()),
                    Token::OpenDelim(DelimToken::Paren),
                    Token::CloseDelim(DelimToken::Paren),
                    Token::Whitespace,
                    Token::OpenDelim(DelimToken::Brace),
                    Token::Whitespace,
                    Token::Ident("fmt".into()),
                    Token::Dot,
                    Token::Ident("Println".into()),
                    Token::OpenDelim(DelimToken::Paren),
                    Token::Literal(Literal::Str("Hello, rgo".into())),
                    Token::CloseDelim(DelimToken::Paren),
                    Token::Whitespace,
                    Token::CloseDelim(DelimToken::Brace),
                    // There's a newline after the closing curly bracket.
                    Token::Whitespace];

    assert_eq_tok(tokenize(src), expected);
}

#[test]
fn tokenize_simple_assignment() {
    assert_eq_tok(tokenize("someVar := 23 + 45"),
                  vec![
               Token::Ident("someVar".into()),
               Token::Whitespace,
               Token::ColonAssign,
               Token::Whitespace,
               Token::Literal(Literal::Integer("23".into())),
               Token::Whitespace,
               Token::Plus,
               Token::Whitespace,
               Token::Literal(Literal::Integer("45".into())),
    ]);
}
