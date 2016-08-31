use token::{TokenKind, Spanned};
use super::{Type, Ident, MaybeQualifiedIdent, Literal, BasicLit, FuncDecl};

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


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuzzyExpr {
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
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

impl BinaryOperator {
    pub fn from_token_kind(tok: TokenKind) -> Option<BinaryOperator> {
        use self::BinaryOperator::*;
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

    pub fn from_token_kind_assign_op(tok: TokenKind) -> Option<BinaryOperator> {
        use self::BinaryOperator::*;
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
        use self::BinaryOperator::*;

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
    pub lhs: Box<Spanned<Expr>>,
    pub op: BinaryOperator,
    pub rhs: Box<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryExpr {
    Primary(Box<PrimaryExpr>),
    UnaryOperation(UnaryOperation),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryOperation {
    pub operator: UnaryOperator, // TODO: type safety
    pub operand: Box<Spanned<UnaryExpr>>,
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
// Conversion = Type "(" Expression [ "," ] ")" .

/// Primary expressions are the operands for unary and binary expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimaryExpr {
    Operand(Operand),
    /// Function calls and type conversions have a similar syntax.
    CallOrConv(CallOrConv),
    SelectorExpr(SelectorExpr),
    Indexing(IndexExpr),
    Slicing(SliceExpr),
    TypeAssertion(TypeAssertion),
}

/// Operands denote the elementary values in an expression. They are themselves expressions.
///
/// An operand may be a literal, a
/// (possibly qualified) non-blank identifier denoting a constant, variable, or function, a method
/// expression yielding a function, or a parenthesized expression.
// XXX/FIXME/TODO: not finished.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    /// A literal.
    Lit(Literal),
    /// An identifier denoting a constant, a variable or a function.
    MaybeQualifiedIdent(MaybeQualifiedIdent),
    /// A method expression.
    MethodExpr(SelectorExpr),
    /// A parenthesized expression.
    Expr(Expr),
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
    pub operand: Box<Spanned<PrimaryExpr>>,
    pub index: Spanned<Expr>,
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
    pub operand: Box<Spanned<PrimaryExpr>>,
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
    pub low: Spanned<Expr>,
    pub high: Spanned<Expr>,
    pub max: Option<Spanned<Expr>>,
}

/// A TypeAssertion contains the expression whose type is being asserted.
/// This superficially differs from the grammar in the Go spec.
// XXX: wrap both fields in Spanned<T>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAssertion {
    /// The expression whose type is being asserted.
    pub expr: Box<PrimaryExpr>,
    /// The 'target type'.
    /// If None, we're in a type switch (`x.(type)` - with the literal `type` keyword).
    pub typ: Option<Type>,
}



#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallOrConv {
    pub callee: Box<FuzzyOperand>,
    pub args: Vec<Expr>,
    /// Whether an ellipsis was encountered.
    pub ellipsis: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
// unimplemented!
pub struct Callee;

/// A selector expression.
///
/// This can represent either a field access (`someVar.field`) or a method expression
/// (`Type.MethodName`).
///
/// ## Grammar
///
/// Field access:
///
/// ```ignore
/// SelectorExpr = PrimaryExpr "." identifier .
/// ````
///
/// Method expression:
/// ```ignore
/// MethodExpr    = ReceiverType "." MethodName .
/// ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorExpr {
    pub operand: Box<FuzzyOperand>,
    pub selector: Ident,
}


/// An ugly parsing hack.
#[derive(Debug, Clone, PartialEq, Eq)]
enum FuzzyOperand {
    Ident(Ident),
    BasicLit(BasicLit),
    /// A parenthesized expression or type (rhs or type).
    Paren(FuzzyExpr),
    /// A function type or literal, which has exactly the same syntax as a function
    /// declaration.
    FuncTypeOrLit(FuncDecl),
}
