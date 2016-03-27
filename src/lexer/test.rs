use super::*;

#[test]
fn tokenize_hello() {
    let src = r#"
package main

import "fmt"

func main() {
	fmt.Println("Hello, rgo")
}
"#;

    let expected = vec![Token::Keyword(Keyword::Package),
                        Token::Whitespace,
                        Token::Ident("main".into()),
                        Token::Whitespace,
                        Token::Keyword(Keyword::Import),
                        Token::Whitespace,
                        Token::Literal(Literal::String(StrLit::Interpreted("fmt".into()))),
                        Token::Whitespace,
                        Token::Keyword(Keyword::Func),
                        Token::Whitespace,
                        Token::Ident("main".into()),
                        Token::OpenDelim(DelimToken::Bracket),
                        Token::CloseDelim(DelimToken::Bracket)
                        Token::Whitespace,
                        Token::OpenDelim(DelimToken::Brace),
                        Token::Whitespace,
                        Token::Ident("fmt".into()),
                        Token::Dot,
                        Token::Ident("Println".into()),
                        Token::OpenDelim(DelimToken::Paren),
                        Token::Literal(Literal::String("Hello, rgo".into()))
                        Token::CloseDelim(DelimToken::Paren),
                        Token::Whitespace,
                        Token::CloseDelim(DelimToken::Brace),
    ];
}
