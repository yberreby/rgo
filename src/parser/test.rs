use ast;
use lexer::{Token, Keyword, DelimToken, Literal};
use super::{Parser, parse};

#[test]
fn parse_package_clause() {
    let tokens = vec![Token::Keyword(Keyword::Package), Token::Ident("main".into())];
    let mut parser = Parser::new(tokens);
    assert_eq!(parser.parse_package_clause(), "main".to_owned());
}

#[test]
fn parse_package_clause_whitespace() {
    let tokens = vec![Token::Keyword(Keyword::Package), Token::Ident("main".into())];
    let mut parser = Parser::new(tokens);
    assert_eq!(parser.parse_package_clause(), "main".to_owned());
}

#[test]
fn parse_short_import() {
    let tokens = vec![Token::Keyword(Keyword::Import), Token::Literal(Literal::Str("fmt".into()))];

    let expected = vec![ast::ImportDecl {
                            specs: vec![ast::ImportSpec {
                                            kind: ast::ImportKind::Normal,
                                            path: "fmt".into(),
                                        }],
                        }];

    let mut parser = Parser::new(tokens);
    assert_eq!(parser.parse_import_decls(), expected);
}

#[test]
fn parse_long_import() {
    let tokens = vec![Token::Keyword(Keyword::Import),
                      Token::OpenDelim(DelimToken::Paren),
                      Token::Literal(Literal::Str("github.com/user/stringutil".into())),
                      Token::CloseDelim(DelimToken::Paren)];

    let expected = vec![ast::ImportDecl {
                            specs: vec![ast::ImportSpec {
                                            kind: ast::ImportKind::Normal,
                                            path: "github.com/user/stringutil".into(),
                                        }],
                        }];

    let mut parser = Parser::new(tokens);
    assert_eq!(parser.parse_import_decls(), expected);
}

// Simplest possible Go program (AFAIK).
#[test]
fn parse_simplest() {
    let tokens = vec![Token::Keyword(Keyword::Package),
                      Token::Ident("main".into()),
                      Token::Keyword(Keyword::Func),
                      Token::Ident("main".into()),
                      Token::OpenDelim(DelimToken::Paren),
                      Token::CloseDelim(DelimToken::Paren),
                      Token::OpenDelim(DelimToken::Brace),
                      Token::CloseDelim(DelimToken::Brace)];
    let expected = ast::SourceFile {
        package: "main".into(),
        import_decls: vec![],
        top_level_decls: vec![ast::TopLevelDecl::Func(ast::FuncDecl {
                                  name: "main".into(),
                                  // `main` takes no arguments and returns nothing.
                                  signature: ast::FuncSignature {
                                      parameters: ast::Parameters::empty(),
                                      result: ast::Parameters::empty(),
                                  },
                                  // empty body: no statements.
                                  body: vec![],
                              })],
    };

    assert_eq!(parse(tokens), expected);
}

#[ignore]
#[test]
fn parse_hello() {
    let tokens = vec![Token::Keyword(Keyword::Package),
                      Token::Ident("main".into()),
                      Token::Keyword(Keyword::Import),
                      Token::Literal(Literal::Str("fmt".into())),
                      Token::Keyword(Keyword::Func),
                      Token::Ident("main".into()),
                      Token::OpenDelim(DelimToken::Bracket),
                      Token::CloseDelim(DelimToken::Bracket),
                      Token::OpenDelim(DelimToken::Brace),
                      Token::Ident("fmt".into()),
                      Token::Dot,
                      Token::Ident("Println".into()),
                      Token::OpenDelim(DelimToken::Paren),
                      Token::Literal(Literal::Str("Hello, rgo".into())),
                      Token::CloseDelim(DelimToken::Paren),

                      Token::CloseDelim(DelimToken::Brace)];

    let expected = ast::SourceFile {
        package: "main".into(),
        import_decls: vec![ast::ImportDecl {
                               specs: vec![ast::ImportSpec {
                                               kind: ast::ImportKind::Normal,
                                               path: "fmt".into(),
                                           }],
                           }],
        top_level_decls: vec![ast::TopLevelDecl::Func(ast::FuncDecl {
                                  name: "main".into(),
                                  signature: ast::FuncSignature {
                                      parameters: ast::Parameters::empty(),
                                      result: ast::Parameters::empty(),
                                  },
                                  body: vec![
                ast::Statement::Simple(
                    ast::SimpleStmt::Expression(ast::Expression::Unary(
                            ast::UnaryExpr::Primary(
                                Box::new(ast::PrimaryExpr::FuncCall(
                                    Box::new(unimplemented!()),
                                    vec![
                                    unimplemented!()
                                    ]
                                ))
                            )
                            ))
                )
            ],
                              })],
    };
    assert_eq!(parse(tokens), expected);
}
