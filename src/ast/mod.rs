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
    Unary(UnaryExpr),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryExpr {
    Primary(Box<PrimaryExpr>),
    UnaryOperation(UnaryOperator, Box<UnaryExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {

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
    FuncCall(Box<PrimaryExpr>, Vec<Argument>),
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
}

// XXX: types need attention.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Plain(MaybeQualifiedIdent),
    Literal(TypeLiteral),
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
    pub package: Option<String>,
    pub name: String,
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerType;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceType;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SliceType;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapType;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChanType;

// Statement =
// 	Declaration | LabeledStmt | SimpleStmt |
// 	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
// 	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
// 	DeferStmt .
//
// SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Decl(DeclStmt),
    Labeled(LabeledStmt),
    Simple(SimpleStmt),
    Go(GoStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Goto(GotoStmt),
    Fallthrough(FallthroughStmt),
    Block(Block),
    If(IfStmt),
    Switch(SwitchStmt),
    Select(SelectStmt),
    For(ForStmt),
    Defer(DeferStmt),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleStmt {
    EmptyStmt,
    Expression(Expression),
    Send(SendStmt),
    IncDec(IncDecStmt),
    Assignment(Assignment),
    ShortVarDecl(ShortVarDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LabeledStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GoStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BreakStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContinueStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GotoStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FallthroughStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeferStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SendStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncDecStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShortVarDecl;


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicLit;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompositeLit;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncLit;
