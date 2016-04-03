use lexer::Token;
use ast::*;

pub fn parse_tokens(tokens: Vec<Token>) -> SourceFile {
    unimplemented!()
}


#[test]
fn parse_hello() {
    let tokens = [Token::Keyword(Keyword::Package),
                  Token::Ident("main".into()),
                  Token::Whitespace,
                  Token::Keyword(Keyword::Import),
                  Token::Literal(Literal::Str("fmt".into())),
                  Token::Whitespace,
                  Token::Keyword(Keyword::Func),
                  Token::Ident("main".into()),
                  Token::OpenDelim(DelimToken::Bracket),
                  Token::CloseDelim(DelimToken::Bracket),
                  Token::OpenDelim(DelimToken::Brace),
                  Token::Whitespace,
                  Token::Ident("fmt".into()),
                  Token::Dot,
                  Token::Ident("Println".into()),
                  Token::OpenDelim(DelimToken::Paren),
                  Token::Literal(Literal::Str("Hello, rgo".into())),
                  Token::CloseDelim(DelimToken::Paren),
                  Token::Whitespace,
                  Token::CloseDelim(DelimToken::Brace)];

    let expected = SourceFile {
        package: "main".into(),
        import_decls: vec![ImportDecl {
                               specs: vec![ImportSpec {
                                               alias: None,
                                               path: "fmt".into(),
                                           }],
                           }],
        top_level_decls: vec![unimplemented!()],
    };
    assert_eq!(parse_tokens(tokens), expected);
}
