use super::{Block, Expr, ShortVarDecl, ConstDecl, TypeDecl, VarDecl, BinaryOperation};
use token::Spanned;


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
    ($enum_type:ident, $(($enum_variant:ident, $inner_type:ty)),*) => {
        $(
            impl From<$inner_type> for $enum_type {
                fn from(x: $inner_type) -> $enum_type {
                    $enum_type::$enum_variant(x)
                }
            }
        )*
    }
}

enum_from_impl!(Statement,
                (Decl, DeclStmt),
                (Labeled, LabeledStmt),
                (Simple, SimpleStmt),
                (Go, GoStmt),
                (Return, ReturnStmt),
                (Break, BreakStmt),
                (Continue, ContinueStmt),
                (Goto, GotoStmt),
                (Fallthrough, FallthroughStmt),
                (Block, Block),
                (If, IfStmt),
                (Switch, SwitchStmt),
                (Select, SelectStmt),
                (For, ForStmt),
                (Defer, DeferStmt),
                (Empty, EmptyStmt));


/// A simple statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleStmt {
    EmptyStmt,
    Expr(Spanned<Expr>),
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
    pub call: Spanned<Expr>,
}

/// A "defer" statement invokes a function whose execution is deferred to the moment the
/// surrounding function returns, either because the surrounding function executed a return
/// statement, reached the end of its function body, or because the corresponding goroutine is
/// panicking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeferStmt {
    /// The function or method call being deferred.
    pub call: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnStmt {
    /// The expression being returned.
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BreakStmt {
    /// An optional label referring to an enclosing "for", "switch", or "select".
    pub label: Option<Spanned<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContinueStmt {
    /// An optional label referring to an enclosing "for", "switch", or "select".
    pub label: Option<Spanned<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GotoStmt {
    pub label: Spanned<String>,
}

/// "Fallthrough" statements contain no associated data!
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
    pub condition: Spanned<Expr>,
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
    pub expr: Spanned<Expr>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IterVars {
    Exprs(Vec<Spanned<Expr>>),
    Idents(Vec<Spanned<String>>),
}

// SendStmt = Channel "<-" Expression .
// Channel  = Expression .
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SendStmt {
    pub channel: Spanned<Expr>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncDecStmt {
    pub expr: Spanned<Expr>,
    pub is_dec: bool, // false for ++, true for --
}

// Assignment = ExpressionList assign_op ExpressionList .
// assign_op = [ add_op | mul_op ] "=" .
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
    pub lhs: Vec<Spanned<Expr>>,
    pub rhs: Vec<Spanned<Expr>>,
    // binary operation used in assign op
    // XXX: add method to BinaryOperation to check if is a valid assign_op operation
    pub op: Option<BinaryOperation>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyStmt;

// Declaration   = ConstDecl | TypeDecl | VarDecl .
/// A statement declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclStmt {
    Const(ConstDecl),
    TypeDecl(TypeDecl),
    VarDecl(VarDecl),
}
