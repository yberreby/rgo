// Go language specification: https://golang.org/ref/spec

// SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .

/// A complete source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub package: String,
    pub import_decls: Vec<ImportDecl>,
    pub top_level_decls: Vec<TopLevelDecl>,
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDecl {
    pub specs: Vec<ImportSpec>,
}

/// An import spec.
/// Can only appear in an import declaration (AFAIK).
///
/// Example: `m "lib/math"`
/// This imports lib/math as m.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportSpec {
    pub kind: ImportKind,
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportKind {
    /// Regular import: the kind you encounter most often.
    Normal,
    /// Aliased import: defines an alias for the imported package.
    Alias(String),
    /// Glob import: all the package's exported identifiers will be declared in the importing
    /// source file.
    Glob,
}

// Declaration   = ConstDecl | TypeDecl | VarDecl .
/// A statement declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclStatement {
    Const(ConstDecl),
    TypeDecl(TypeDecl),
    VarDecl(VarDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A top-level declaration - i.e. a declaration that may appear immediately after import
/// declarations.
pub enum TopLevelDecl {
    Statement(DeclStatement),
    Func(FuncDecl),
    Method(MethodDecl),
}

// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
// ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
//
// IdentifierList = identifier { "," identifier } .
// ExpressionList = Expression { "," Expression } .

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstSpec {
    pub identifiers: Vec<Identifier>,
    pub typ: Option<Type>,
    pub expressions: Vec<Expression>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    UnaryExpr(UnaryExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
// Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")".

/// Primary expressions are the operands for unary and binary expressions. 
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Slice {

}



// From the Go spec:
//
// FunctionDecl = "func" FunctionName ( Function | Signature ) .
// FunctionName = identifier .
// Function     = Signature FunctionBody .
// FunctionBody = Block .
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDecl {
    // XXX: functions with same name but different origins, how do we handle them?
    pub name: String,
    pub signature: FuncSignature,
    pub body: Vec<Statement>,
}


/// A function signature: return type(s) and argument types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncSignature {
    pub parameters: Parameters,
    // Yes, the result of a function is a `Parameters` struct.
    pub result: Parameters,
}


pub struct Parameters {
    lists: Vec<ParameterDecl>,
}

// XXX: variadic functions.

pub struct ParameterDecl {
    identifiers: Vec<String>,
}

// XXX - typedef, newtype or something else?
pub type Type = String;

// == Unimplemented types ==

/// A constant declaration.
///
/// Example: `const Pi float64 = 3.14159265358979323846`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstDecl;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDecl;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDecl;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDecl;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operand;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Conversion;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Argument;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {}


// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statement;
