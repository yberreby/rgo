use lexer::TokenKind;
use super::{Arguments, Operand, Conversion, Type, Ident};

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

    pub fn from_token_kind_assign_op(tok: TokenKind) -> Option<BinaryOperation> {
        use self::BinaryOperation::*;
        Some(match tok {
            TokenKind::PlusAssign => Add,
            TokenKind::MinusAssign => Sub,
            TokenKind::StarAssign => Mul,
            TokenKind::SlashAssign => Div,
            TokenKind::PercentAssign => Rem,

            TokenKind::AndAssign => BitAnd,
            TokenKind::OrAssign => BitOr,
            TokenKind::CaretAssign => BitXor,
            TokenKind::BitClearAssign => BitClear,

            TokenKind::LshiftAssign => LeftShift,
            TokenKind::RshiftAssign => RightShift,

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
/// A unary operator.
///
/// ## Grammar
///
/// ```ignore
/// unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
    Xor,
    Deref,
    And,
    ChanReceive,
}

impl UnaryOperator {
    pub fn from_token_kind(k: TokenKind) -> Option<UnaryOperator> {
        use self::UnaryOperator::*;

        Some(match k {
            TokenKind::Plus => Plus,
            TokenKind::Minus => Minus,
            TokenKind::Not => Not,
            TokenKind::Caret => Xor,
            TokenKind::Star => Deref,
            TokenKind::And => And,
            TokenKind::Arrow => ChanReceive,
            _ => return None,
        })
    }
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


/// A selector expression.
///
/// ## Grammar
///
/// ```ignore
/// SelectorExpr = PrimaryExpr "." identifier .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorExpr {
    pub operand: Box<PrimaryExpr>,
    pub selector: Ident,
}

/// An index expression.
///
/// ## Grammar
///
/// ```ignore
/// IndexExpr = PrimaryExpr "[" Expression "]" .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexExpr {
    pub operand: Box<PrimaryExpr>,
    pub index: Expr,
}


/// A slice expression.
///
/// ## Grammar
///
/// ```ignore
/// SliceExpr = PrimaryExpr "[" ( [ Expression ] ":" [ Expression ] ) |
///                             ( [ Expression ] ":" Expression ":" Expression )
///                         "]" .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SliceExpr {
    pub operand: Box<PrimaryExpr>,
    pub slicing: Slicing,
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
    pub expr: Box<PrimaryExpr>,
    pub typ: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncCall {
    pub callee: Box<PrimaryExpr>,
    pub args: Arguments,
}



/// A method expression.
///
/// If M is in the method set of type T, T.M is a function that is callable as a regular function
/// with the same arguments as M prefixed by an additional argument that is the receiver of the
/// method.
///
/// ## Grammar
///
/// ```ignore
/// MethodExpr    = ReceiverType "." MethodName .
/// ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodExpr {
    /// Receiver type.
    pub receiver: Type,
    /// Name of the method.
    pub name: String,
}
