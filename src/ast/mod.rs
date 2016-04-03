// Go language specification: https://golang.org/ref/spec

// SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .

/// A complete source file.
pub struct SourceFile {
    package: String,
    import_decls: Vec<ImportDecl>,
    top_level_decls: Vec<TopLevelDecl>,
}

// ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
// ImportSpec       = [ "." | PackageName ] ImportPath .
// ImportPath       = string_lit .

/// An import declaration.
/// Contains a list of "import specs".
///
/// Example:
///
/// ``` go
/// import (
///     "fmt"
///     "io/ioutil"
/// )
/// ```
pub struct ImportDecl {
    specs: Vec<ImportSpec>,
}

/// An import spec.
/// Can only appear in an import declaration (AFAIK).
///
/// Example: `m "lib/math"`
/// This imports lib/math as m.
pub struct ImportSpec {
    alias: Option<String>,
    path: String,
}

// Declaration   = ConstDecl | TypeDecl | VarDecl .
/// A regular (i.e. not top-level) declaration.
pub enum Declaration {
    Const(ConstDecl),
    TypeDecl,
    VarDecl,
}



// TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
pub enum TopLevelDecl {
    Declaration(Declaration),
    FunctionDecl(FunctionDecl),
    MethodDecl(MethodDecl),
}

// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
// ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
//
// IdentifierList = identifier { "," identifier } .
// ExpressionList = Expression { "," Expression } .

pub struct ConstSpec {
    identifiers: Vec<Identifier>,
    typ: Option<Typ>,
    expressions: Vec<Expression>,
}


// Expression = UnaryExpr | Expression binary_op Expression .
// UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
//
// binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
// rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
// add_op     = "+" | "-" | "|" | "^" .
// mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
//
// unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .

pub enum Expression {
    UnaryExpr(UnaryExpr),
}

pub enum UnaryExpr {
    PrimaryExpr(Box<PrimaryExpr>),
    UnaryOperation(UnaryOperator, Box<UnaryExpr>),
}

// pub enum


// Primary expressions.

// PrimaryExpr =
// 	Operand |
// 	Conversion |
// 	PrimaryExpr Selector |
// 	PrimaryExpr Index |
// 	PrimaryExpr Slice |
// 	PrimaryExpr TypeAssertion |
// 	PrimaryExpr Arguments .
//
// Selector       = "." identifier .
// Index          = "[" Expression "]" .
// Slice          = "[" ( [ Expression ] ":" [ Expression ] ) |
//                      ( [ Expression ] ":" Expression ":" Expression )
//                  "]" .
// TypeAssertion  = "." "(" Type ")" .
// Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .

/// Primary expressions are the operands for unary and binary expressions. 
pub enum PrimaryExpr {
    Operand(Operand),
    Conversion(Conversion),
    Selection(Box<PrimaryExpr>, String),
    Indexing(Box<PrimaryExpr>, Expression),
    Slicing(Box<PrimaryExpr>, Slice),
    TypeAssertion(Box<PrimaryExpr>, String),
    FunctionCall(Box<PrimaryExpr>, Vec<Argument>),
}

pub fn parse_primary_expr(s: &str) -> PrimaryExpr {
    unimplemented!()
}

// Represents a slicing operating... [1:54] for ex
pub enum Slice {

}


// == Unimplemented types ==

/// A constant declaration.
///
/// Example: `const Pi float64 = 3.14159265358979323846`
pub struct ConstDecl;
pub struct FunctionDecl;
pub struct MethodDecl;
pub struct Identifier;
pub struct Typ;
pub struct TypeDecl;
pub struct VarDecl;
pub struct Operand;
pub struct Conversion;
pub struct Argument;
pub enum UnaryOperator {}
