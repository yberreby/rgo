// Go language specification: https://golang.org/ref/spec

use num::bigint::BigInt;
use num::BigRational;
use token::TokenKind;

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

// Declaration   = ConstDecl | TypeDecl | VarDecl .
/// A statement declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclStmt {
    Const(ConstDecl),
    TypeDecl(TypeDecl),
    VarDecl(VarDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A top-level declaration - i.e. a declaration that may appear immediately after import
/// declarations.
pub enum TopLevelDecl {
    Statement(DeclStmt),
    Func(FuncDecl),
    Method(MethodDecl),
}

// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
// ConstSpec      = IdentifierList [ [ Type ] "=" ExprList ] .
//
// IdentifierList = identifier { "," identifier } .
// ExprList = Expr { "," Expr } .

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstSpec {
    pub identifiers: Vec<Identifier>,
    pub typ: Option<Type>,
    pub expressions: Vec<Expr>,
}


// Expr = UnaryExpr | Expr binary_op Expr .
// UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
//
// binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
// rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
// add_op     = "+" | "-" | "|" | "^" .
// mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
//
// unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .

// I went for a strongly-typed approach here: instead of having one giant, flat `Expr` enum, I
// chose a deeply nested hierarchy.
//
// Advantage: more type-safety.
// Disadvantages:
// - takes up more space because Rust doesn't collapse nested enum tags
// - more verbose

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    BitAnd,
    BitOr,
    BitXor,
    BitClear,

    LeftShift,
    RightShift,

    Equals,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogAnd,
    LogOr,
}

impl BinaryOperation {
    pub fn from_token_kind(tok: TokenKind) -> Option<BinaryOperation> {
        use self::BinaryOperation::*;
        Some(match tok {
            TokenKind::Plus => Add,
            TokenKind::Minus => Sub,
            TokenKind::Star => Mul,
            TokenKind::Slash => Div,
            TokenKind::Percent => Rem,

            TokenKind::And => BitAnd,
            TokenKind::Or => BitOr,
            TokenKind::Caret => BitXor,
            TokenKind::BitClear => BitClear,

            TokenKind::Lshift => LeftShift,
            TokenKind::Rshift => RightShift,

            TokenKind::Equals => Equals,
            TokenKind::NotEqual => NotEqual,
            TokenKind::LessThan => LessThan,
            TokenKind::LessThanOrEqual => LessThanOrEqual,
            TokenKind::GreaterThan => GreaterThan,
            TokenKind::GreaterThanOrEqual => GreaterThanOrEqual,
            TokenKind::AndAnd => LogAnd,
            TokenKind::OrOr => LogOr,

            _ => return None,
        })
    }

    pub fn precedence(self) -> i32 {
        use self::BinaryOperation::*;

        match self {
            Mul | Div | Rem | LeftShift | RightShift | BitAnd | BitClear => 5,
            Add | Sub | BitOr | BitXor => 4,
            Equals | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => 3,
            LogAnd => 2,
            LogOr => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOperation,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryExpr {
    Primary(Box<PrimaryExpr>),
    UnaryOperation(UnaryOperation),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryOperation {
    pub operator: UnaryOperator, // TODO: type safety
    pub operand: Box<UnaryExpr>,
}

// TODO
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
    Caret,
    Star,
    And,
    Arrow, // ... not sure about this one
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
// Index          = "[" Expr "]" .
// Slice          = "[" ( [ Expr ] ":" [ Expr ] ) |
//                      ( [ Expr ] ":" Expr ":" Expr )
//                  "]" .
// TypeAssertion  = "." "(" Type ")" .
// Arguments      = "(" [ ( ExprList | Type [ "," ExprList ] ) [ "..." ] [ "," ] ] ")".

/// Primary expressions are the operands for unary and binary expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimaryExpr {
    Operand(Operand),
    Conversion(Conversion),
    SelectorExpr(SelectorExpr),
    Indexing(IndexExpr),
    Slicing(SliceExpr),
    TypeAssertion(TypeAssertion),
    FuncCall(FuncCall),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorExpr {
    pub operand: Box<PrimaryExpr>,
    pub selector: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexExpr {
    pub operand: Box<PrimaryExpr>,
    pub index: Expr,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SliceExpr {
    operand: Box<PrimaryExpr>,
    slicing: Slicing,
}
// XXX: naming

// From the Go spec:
//
//  For an array, pointer to array, or slice a (but not a string), the primary expression
//
//    a[low : high : max]
//
// constructs a slice of the same type, and with the same length and elements as the simple slice
// expression a[low : high]. Additionally, it controls the resulting slice's capacity by setting it
// to max - low. Only the first index may be omitted; it defaults to 0. After slicing the array a
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Slicing {
    pub low: Expr,
    pub high: Expr,
    pub max: Option<Expr>,
}

/// A TypeAssertion contains the expression whose type is being asserted.
/// This superficially differs from the grammar in the Go spec.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAssertion {
    expr: Box<PrimaryExpr>,
    typ: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncCall {
    callee: Box<PrimaryExpr>,
    args: Arguments,
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

/// Operands denote the elementary values in an expression. An operand may be a literal, a
/// (possibly qualified) non-blank identifier denoting a constant, variable, or function, a method
/// expression yielding a function, or a parenthesized expression.
// XXX/FIXME/TODO: not finished.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Lit(Literal),
    /// A constant, a variable or a function.
    Ident(MaybeQualifiedIdent),
    __Todo,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Conversion;


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
// SimpleStmt = EmptyStmt | ExprStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .

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
    Empty(EmptyStmt),
}

macro_rules! enum_from_impl {
    ($enum_type:ident, $enum_variant:ident, $inner_type:ty) => {
        impl From<$inner_type> for $enum_type {
            fn from(x: $inner_type) -> $enum_type {
                $enum_type::$enum_variant(x)
            }
        }
    }
}

enum_from_impl!(Statement, Decl, DeclStmt);
enum_from_impl!(Statement, Labeled, LabeledStmt);
enum_from_impl!(Statement, Simple, SimpleStmt);
enum_from_impl!(Statement, Go, GoStmt);
enum_from_impl!(Statement, Return, ReturnStmt);
enum_from_impl!(Statement, Break, BreakStmt);
enum_from_impl!(Statement, Continue, ContinueStmt);
enum_from_impl!(Statement, Goto, GotoStmt);
enum_from_impl!(Statement, Fallthrough, FallthroughStmt);
enum_from_impl!(Statement, Block, Block);
enum_from_impl!(Statement, If, IfStmt);
enum_from_impl!(Statement, Switch, SwitchStmt);
enum_from_impl!(Statement, Select, SelectStmt);
enum_from_impl!(Statement, For, ForStmt);
enum_from_impl!(Statement, Defer, DeferStmt);
enum_from_impl!(Statement, Empty, EmptyStmt);


/// A simple statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleStmt {
    EmptyStmt,
    Expr(Expr),
    Send(SendStmt),
    IncDec(IncDecStmt),
    Assignment(Assignment),
    ShortVarDecl(ShortVarDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LabeledStmt;

/// A "go" statement starts the execution of a function call as an independent concurrent thread of
/// control, or goroutine, within the same address space.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GoStmt {
    /// The function or method call being started.
    pub call: Expr,
}

/// A "defer" statement invokes a function whose execution is deferred to the moment the
/// surrounding function returns, either because the surrounding function executed a return
/// statement, reached the end of its function body, or because the corresponding goroutine is
/// panicking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeferStmt {
    /// The function or method call being deferred.
    pub call: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnStmt {
    /// The expression being returned.
    pub expr: Expr,
}




#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BreakStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContinueStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GotoStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FallthroughStmt;


/// "If" statements specify the conditional execution of two branches according to the value of a
/// boolean expression. If the expression evaluates to true, the "if" branch is executed,
/// otherwise, if present, the "else" branch is executed.
///
/// The expression may be preceded by a simple statement, which executes before the expression is
/// evaluated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStmt {
    pub before_stmt: Option<SimpleStmt>,
    pub condition: Expr,
    pub block: Block,
    pub opt_else: Option<Box<Else>>,
}

/// The "else" portion of an if statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Else {
    /// `else if <condition> { ... }`
    If(IfStmt),
    /// `else { ... }`
    Block(Block),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectStmt;


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForStmt {
    /// The "header" is the part of of a `for` that comes before the body.
    pub header: ForHeader,
    pub body: Block,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForHeader {
    Condition(Expr),
    ForClause(ForClause),
    RangeClause(RangeClause),
}

// Grammar:
//
// ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
// InitStmt = SimpleStmt .
// PostStmt = SimpleStmt .
// Condition = Expression .
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForClause {
    pub init: Option<SimpleStmt>,
    pub condition: Option<Expr>,
    pub post: Option<SimpleStmt>,
}


// RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RangeClause {
    /// The iteration variables.
    pub iter_vars: IterVars,
    /// The range expression.
    pub expr: Expr,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IterVars {
    Exprs(Vec<Expr>),
    Idents(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SendStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncDecStmt;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment;

// ShortVarDecl = IdentifierList ":=" ExpressionList .
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShortVarDecl {
    pub lhs: Vec<String>,
    pub rhs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyStmt;

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
