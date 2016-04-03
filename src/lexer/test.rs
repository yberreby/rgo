use super::{Token, DelimToken, Literal, Keyword, tokenize};

/// Utility function to compare two slices of tokens.
/// Better than `assert_eq!` because it shows the mismatch more clearly.
fn tok_cmp<T, U>(found_s: T, expected_s: U)
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
    tok_cmp(tokenize("("), vec![Token::OpenDelim(DelimToken::Paren)]);
    tok_cmp(tokenize(")"), vec![Token::CloseDelim(DelimToken::Paren)]);
    tok_cmp(tokenize("{"), vec![Token::OpenDelim(DelimToken::Brace)]);
    tok_cmp(tokenize("}"), vec![Token::CloseDelim(DelimToken::Brace)]);
    tok_cmp(tokenize("["), vec![Token::OpenDelim(DelimToken::Bracket)]);
    tok_cmp(tokenize("]"), vec![Token::CloseDelim(DelimToken::Bracket)]);
}

#[test]
fn tokenize_comma() {
    tok_cmp(tokenize(","), vec![Token::Comma]);
}

#[test]
fn tokenize_semicolon() {
    tok_cmp(tokenize(";"), vec![Token::Semicolon]);
}

#[test]
fn tokenize_dot_variants() {
    tok_cmp(tokenize("."), vec![Token::Dot]);
    tok_cmp(tokenize("..."), vec![Token::Ellipsis]);
}

#[test]
fn tokenize_pipe_variants() {
    tok_cmp(tokenize("|"), vec![Token::Or]);
    tok_cmp(tokenize("||"), vec![Token::OrOr]);
    tok_cmp(tokenize("|="), vec![Token::OrAssign]);
}

#[test]
fn tokenize_not_variants() {
    tok_cmp(tokenize("!"), vec![Token::Not]);
    tok_cmp(tokenize("!="), vec![Token::NotEqual]);
}

#[test]
fn tokenize_caret_variants() {
    tok_cmp(tokenize("^"), vec![Token::Caret]);
    tok_cmp(tokenize("^="), vec![Token::CaretAssign]);
}

#[test]
fn tokenize_percent_variants() {
    tok_cmp(tokenize("%"), vec![Token::Percent]);
    tok_cmp(tokenize("%="), vec![Token::PercentAssign]);
}

#[test]
fn tokenize_and_variants() {
    tok_cmp(tokenize("&"), vec![Token::And]);
    tok_cmp(tokenize("&&"), vec![Token::AndAnd]);
    tok_cmp(tokenize("&="), vec![Token::AndAssign]);
    tok_cmp(tokenize("&^"), vec![Token::BitClear]);
    tok_cmp(tokenize("&^="), vec![Token::BitClearAssign]);
}

#[test]
fn tokenize_plus_variants() {
    tok_cmp(tokenize("+"), vec![Token::Plus]);
    tok_cmp(tokenize("++"), vec![Token::Increment]);
    tok_cmp(tokenize("+="), vec![Token::PlusAssign]);
}

#[test]
fn tokenize_minus_variants() {
    tok_cmp(tokenize("-"), vec![Token::Minus]);
    tok_cmp(tokenize("--"), vec![Token::Decrement]);
    tok_cmp(tokenize("-="), vec![Token::MinusAssign]);
}


#[test]
fn tokenize_colon_variants() {
    tok_cmp(tokenize(":"), vec![Token::Colon]);
    tok_cmp(tokenize(":="), vec![Token::ColonAssign]);
}

#[test]
fn tokenize_lt_variants() {
    tok_cmp(tokenize("<"), vec![Token::LessThan]);
    tok_cmp(tokenize("<-"), vec![Token::ChanReceive]);
    tok_cmp(tokenize("<="), vec![Token::LessThanOrEqual]);
    tok_cmp(tokenize("<<"), vec![Token::Lshift]);
    tok_cmp(tokenize("<<="), vec![Token::LshiftAssign]);
}

#[test]
fn tokenize_gt_variants() {
    tok_cmp(tokenize(">"), vec![Token::GreaterThan]);
    tok_cmp(tokenize(">="), vec![Token::GreaterThanOrEqual]);
    tok_cmp(tokenize(">>"), vec![Token::Rshift]);
    tok_cmp(tokenize(">>="), vec![Token::RshiftAssign]);
}

#[test]
fn tokenize_star_variants() {
    tok_cmp(tokenize("*"), vec![Token::Star]);
    tok_cmp(tokenize("*="), vec![Token::StarAssign]);
}

#[test]
fn tokenize_equal_variants() {
    tok_cmp(tokenize("="), vec![Token::Assign]);
    tok_cmp(tokenize("=="), vec![Token::Equals]);
}

// From the Go spec:
// "A general comment containing no newlines acts like a space. Any other comment acts like a
// newline."
#[ignore]
#[test]
fn tokenize_comments() {
    tok_cmp(tokenize("// Hello, this is a comment"),
            vec![Token::Whitespace]);
    tok_cmp(tokenize("foo /* this is a general comment */ := 2"),
            vec![Token::Ident("foo".into()),
                 Token::ColonAssign,
                 Token::Literal(Literal::Integer("2".into()))]);
}

#[test]
fn tokenize_slash_variants() {
    tok_cmp(tokenize("/"), vec![Token::Slash]);
    tok_cmp(tokenize("/="), vec![Token::SlashAssign]);
}

#[test]
fn tokenize_ident() {
    let test_ident = |s| {
        tok_cmp(tokenize(s), vec![Token::Ident(s.into())]);
    };

    test_ident("foo");
}

#[test]
fn tokenize_keywords() {
    let test_keyword = |s, k| {
        tok_cmp(tokenize(s), vec![Token::Keyword(k)]);
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
    tok_cmp(tokenize(" \t

                        \t  "),
            vec![Token::Whitespace]);
}

#[test]
fn tokenize_package_declaration() {
    tok_cmp(tokenize("package main"),
            vec![Token::Keyword(Keyword::Package), Token::Ident("main".into())]);
}

#[test]
fn tokenize_plain_interpreted_str() {
    tok_cmp(tokenize("\"hello\""),
            vec![Token::Literal(Literal::Str("hello".into()))]);
}

#[test]
fn tokenize_simple_import() {
    tok_cmp(tokenize("import \"fmt\""),
            vec![Token::Keyword(Keyword::Import), Token::Literal(Literal::Str("fmt".into()))]);
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
                    Token::Ident("main".into()),
                    Token::Whitespace,
                    Token::Keyword(Keyword::Import),
                    Token::Literal(Literal::Str("fmt".into())),
                    Token::Whitespace,
                    Token::Keyword(Keyword::Func),
                    Token::Ident("main".into()),
                    Token::OpenDelim(DelimToken::Paren),
                    Token::CloseDelim(DelimToken::Paren),
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

    tok_cmp(tokenize(src), expected);
}

#[test]
fn tokenize_simple_assignment() {
    tok_cmp(tokenize("someVar := 23 + 45"),
            vec![
               Token::Ident("someVar".into()),
               Token::ColonAssign,
               Token::Literal(Literal::Integer("23".into())),
               Token::Plus,
               Token::Literal(Literal::Integer("45".into())),
    ]);
}

#[test]
fn tokenize_string_escape() {
    tok_cmp(tokenize(r##""\\\"oqdz""##),
            vec![Token::Literal(Literal::Str("\\\\\\\"oqdz".into()))]);
}

// =====
// Comments
// =====

#[test]
fn tokenize_simple_assignment_with_inline_comment() {
    tok_cmp(tokenize("someVar /* someVar is a variable; and I'm a COMMENT! */ := 23 + 45"),
            vec![
               Token::Ident("someVar".into()),
               Token::ColonAssign,
               Token::Literal(Literal::Integer("23".into())),
               Token::Plus,
               Token::Literal(Literal::Integer("45".into())),
    ]);
}

#[test]
fn tokenize_hello_with_comments() {
    let src = r#"// This is a line comment.
// And another!
// All of these should be treated as a single contiguous whitespace block.

// Even this one!

package main

import "fmt"

func main() {
	fmt.Println("Hello, rgo")
}
"#;

    let expected = [Token::Whitespace,
                    Token::Keyword(Keyword::Package),
                    Token::Ident("main".into()),
                    Token::Whitespace,
                    Token::Keyword(Keyword::Import),
                    Token::Literal(Literal::Str("fmt".into())),
                    Token::Whitespace,
                    Token::Keyword(Keyword::Func),
                    Token::Ident("main".into()),
                    Token::OpenDelim(DelimToken::Paren),
                    Token::CloseDelim(DelimToken::Paren),
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

    tok_cmp(tokenize(src), expected);
}
