//! The Abstract Syntax Tree.
//!
//! If you want to learn more, it is recommended to read the [Go language
//! specification](https://golang.org/ref/spec).


mod types;
mod statements;
mod expressions;

use num::bigint::BigInt;
use num::BigRational;
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
    pub import_decls: Vec<ImportDecl>,
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
    pub specs: Vec<ImportSpec>,
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
    pub path: Vec<u8>,
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
    pub name: String,
    pub signature: FuncSignature,
    pub body: Block,
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

// XXX: dubious pattern. "Maybe<Something>" does not _feel_ completely right, but it doesn't feel
// _wrong_ either. I just don't see a better solution.

/// A _potentially_ qualified identifier (e.g. `math.Sin`, but also `someUnqualifiedIdent`).
///
/// "A qualified identifier is an identifier qualified with a package name prefix."
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaybeQualifiedIdent {
    pub package: Option<Ident>,
    pub name: Ident,
}

// == Unimplemented types ==

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
    pub idents: Vec<Ident>,
    pub inner: Option<ConstSpecInner>,
}

// XXX: naming
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstSpecInner {
    pub typ: Option<Type>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDecl;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDecl;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDecl;

/// Operands denote the elementary values in an expression. An operand may be a literal, a
/// (possibly qualified) non-blank identifier denoting a constant, variable, or function, a method
/// expression yielding a function, or a parenthesized expression.
// XXX/FIXME/TODO: not finished.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    /// A literal.
    Lit(Literal),
    /// An identifier denoting  a constant, a variable or a function.
    Ident(MaybeQualifiedIdent),
    /// A method expression.
    MethodExpr(MethodExpr),
    /// A parenthesized expression.
    Expr(Expr),
}

/// Conversions are expressions of the form T(x) where T is a type and x is an expression that can
/// be converted to type T.
///
/// ## Grammar
///
/// ```ignore
/// Conversion = Type "(" Expression [ "," ] ")" .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Conversion {
    /// The type to convert to.
    pub typ: Type,
    /// The expression being converted.
    pub expr: Expr,
}


// ShortVarDecl = IdentifierList ":=" ExpressionList .
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShortVarDecl {
    pub lhs: Vec<Ident>,
    pub rhs: Vec<Expr>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompositeLit;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncLit;

#[derive(Debug, Clone, PartialEq, Eq)]
/// A list of arguments being passed to a function.
///
/// Arguments can't just be a list of expressions, because Go has a built-in generic generic
/// functions: `make` and `new`, and these functions take a **type** instead of an expression
/// as their first argument.
pub struct Arguments {
    pub typ: Option<Type>,
    pub expressions: Vec<Expr>,
}
