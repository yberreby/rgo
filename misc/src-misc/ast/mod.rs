//! The Abstract Syntax Tree.
//!
//! If you want to learn more, it is recommended to read the [Go language
//! specification](https://golang.org/ref/spec).
//!
//! This module needs attention.


mod types;
mod statements;
mod expressions;

use num::bigint::BigInt;
use num::BigRational;
use token::Spanned;
pub use self::types::*;
pub use self::statements::*;
pub use self::expressions::*;


// XXX: We may want to intern strings later on.
// XXX: should we use aliases? Or resolve them in doc comments?
pub type Ident = String;
pub type TypeName = MaybeQualifiedIdent;
pub type MethodName = Ident;

/// A complete source file.
///
/// ## Grammar
///
/// ```ignore
/// SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    /// Name of the package this file belongs to.
    pub package: Ident,
    /// All import declarations in this file.
    pub import_decls: Vec<Spanned<ImportDecl>>,
    /// All top-level declarations in this file.
    pub top_level_decls: Vec<TopLevelDecl>,
}

/// An import declaration.
/// Contains a list of "import specs".
///
/// ## Grammar
///
/// ```ignore
/// ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
/// ```
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
    pub specs: Vec<Spanned<ImportSpec>>,
}

/// An import spec.
///
/// This can only appear in an import declaration (AFAIK).
///
/// ## Grammar
///
/// ```ignore
/// ImportSpec       = [ "." | PackageName ] ImportPath .
/// ImportPath       = string_lit .
/// ```
///
/// Example:
///
/// ```ignore
/// m "lib/math"
/// ```
///
/// This imports `lib/math` as `m`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportSpec {
    pub kind: ImportKind,
    pub path: Spanned<Vec<u8>>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
/// A top-level declaration - i.e. a declaration that may appear immediately after import
/// declarations.
pub enum TopLevelDecl {
    Statement(DeclStmt),
    Func(FuncDecl),
    Method(MethodDecl),
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
    pub name: Spanned<String>,
    pub signature: FuncSignature,
    pub body: Option<Block>,
}


/// A function signature: return type(s) and argument types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncSignature {
    pub parameters: Parameters,
    // Yes, the result of a function is a `Parameters` struct.
    pub result: Parameters,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameters {
    pub decls: Vec<ParameterDecl>,
}

impl Parameters {
    /// Create an empty parameter list.
    pub fn empty() -> Parameters {
        Parameters { decls: Vec::new() }
    }

    /// Create a parameter list containing a single, unnamed type.
    pub fn from_single_type(t: Type) -> Parameters {
        Parameters {
            decls: vec![ParameterDecl {
                            identifiers: vec![],
                            typ: t,
                            variadic: false,
                        }],
        }
    }
}

// TODO: variadic functions.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterDecl {
    pub identifiers: Vec<String>,
    /// The type assigned to every identifier in this declaration.
    pub typ: Type,
    // XXX: review this.
    // ONLY the last ParameterDecl of a Parameters struct may be variadic.
    // And only if it's part of _input_ parameters.
    pub variadic: bool,
}

// XXX: types need attention.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Plain(MaybeQualifiedIdent),
    Literal(Box<TypeLiteral>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeLiteral {
    Array(ArrayType),
    Struct(StructType),
    Pointer(PointerType),
    Func(FuncType),
    Interface(InterfaceType),
    Slice(SliceType),
    Map(MapType),
    Chan(ChanType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Basic(BasicLit),
    Composite(CompositeLit),
    Func(FuncLit),
}

/// A _potentially_ qualified identifier (e.g. `math.Sin`, but also `someUnqualifiedIdent`).
///
/// "A qualified identifier is an identifier qualified with a package name prefix."
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaybeQualifiedIdent {
    pub package: Option<Ident>,
    pub name: Ident,
}

/// A constant declaration binds a list of identifiers (the names of the constants) to the values
/// of a list of constant expressions.
///
/// ## Grammar
///
/// ```ignore
/// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
/// ```
///
/// Example: `const Pi float64 = 3.14159265358979323846`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstDecl {
    pub specs: Vec<ConstSpec>,
}


// TODO: docs
/// ## Grammar
///
/// ```ignore
/// ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstSpec {
    pub idents: Vec<Spanned<Ident>>,
    pub inner: Option<ConstSpecInner>,
}

// XXX: naming
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstSpecInner {
    pub typ: Option<Type>,
    pub exprs: Vec<Expr>,
}


/// A method is a function with a receiver. A method declaration binds an identifier, the method
/// name, to a method, and associates the method with the receiver's base type.
///
/// ## Grammar
///
/// ```ignore
/// MethodDecl   = "func" Receiver MethodName ( Function | Signature ) .
/// Receiver     = Parameters .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDecl {
    pub receiver: Parameters,
    pub name: Spanned<Ident>,
    pub signature: FuncSignature,
    pub body: Option<Block>,
}

/// A type declaration binds an identifier, the type name, to a new type that has the same
/// underlying type as an existing type, and operations defined for the existing type are also
/// defined for the new type.
///
/// ## Grammar
///
/// ```ignore
/// TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
/// TypeSpec     = identifier Type .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDecl {
    pub specs: Vec<Spanned<TypeSpec>>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSpec {
    pub ident: Spanned<Ident>,
    pub typ: Spanned<Type>,
}

/// A variable declaration creates one or more variables, binds corresponding identifiers to them,
/// and gives each a type and an initial value.
///
/// ## Grammar
///
/// ```ignore
/// VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDecl {
    pub specs: Vec<Spanned<VarSpec>>,
}

/// ## Grammar
///
/// ```ignore
/// VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarSpec {
    pub idents: Vec<Spanned<Ident>>,
    pub typ: Option<Type>,
    pub exprs: Vec<Spanned<Expr>>,
}

// ShortVarDecl = IdentifierList ":=" ExpressionList .
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShortVarDecl {
    pub lhs: Vec<Spanned<Ident>>,
    pub rhs: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block(pub Vec<Statement>);


// XXX/FIXME: review and fix this.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BasicLit {
    Int(BigInt),
    Float(BigRational),
    Imaginary(BigRational),
    Rune(char),
    Str(Vec<u8>),
}


/// Composite literals construct values for structs, arrays, slices, and maps and create a new
/// value each time they are evaluated. They consist of the type of the literal followed by a
/// brace-bound list of elements. Each element may optionally be preceded by a corresponding key.
///
/// ## Grammar
///
/// ```ignore
/// CompositeLit  = LiteralType LiteralValue .
/// LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
///                 SliceType | MapType | TypeName .
/// LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
/// ElementList   = KeyedElement { "," KeyedElement } .
/// KeyedElement  = [ Key ":" ] Element .
/// Key           = FieldName | Expression | LiteralValue .
/// FieldName     = identifier .
/// Element       = Expression | LiteralValue .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompositeLit {
    pub typ: Spanned<LiteralType>,
    pub val: LiteralValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralType {
    Struct(StructType),
    Array(ArrayType),
    // FIXME: MISSING: `[...]int` array (computes size at compile time)
    Slice(SliceType),
    Map(MapType),
    Type(MaybeQualifiedIdent),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LiteralValue {
    pub elems: Vec<KeyedLiteralElem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyedLiteralElem {
    pub key: Option<Spanned<LiteralKey>>,
    pub elem: Spanned<LiteralElem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralKey {
    FieldName(Ident),
    Expr(Expr),
    LiteralValue(LiteralValue),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralElem {
    Expr(Expr),
    LiteralValue(LiteralValue),
}


/// A function literal represents an anonymous function.
///
/// ## Grammar
///
/// ```ignore
/// FunctionLit = "func" Function .
/// Function     = Signature FunctionBody .
/// FunctionBody = Block .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncLit {
    pub signature: FuncSignature,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A list of arguments being passed to a function.
///
/// Arguments can't just be a list of expressions, because Go has a built-in generic generic
/// functions: `make` and `new`, and these functions take a **type** instead of an expression
/// as their first argument.
pub struct Arguments {
    pub typ: Option<Spanned<Type>>,
    pub expressions: Vec<Spanned<Expr>>,
}
