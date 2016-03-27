// Go language specification: https://golang.org/ref/spec

// SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
pub struct SourceFile {
    package: String,
    import_decls: Vec<ImportDecl>,
    top_level_decls: Vec<TopLevelDecl>,
}

// ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
// ImportSpec       = [ "." | PackageName ] ImportPath .
// ImportPath       = string_lit .

pub struct ImportDecl {
    specs: Vec<ImportSpec>,
}

pub struct ImportSpec {
    alias: Option<String>,
    path: String,
}

// Declaration   = ConstDecl | TypeDecl | VarDecl .
pub enum Declaration {
    ConstDecl,
    TypeDecl,
    VarDecl,
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
    PrimaryExpr(PrimaryExpr),
    UnaryOperation(UnaryOperator, UnaryExpr),
}

// pub enum


// TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
pub enum TopLevelDecl {
    Declaration(Decl),
}



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
    Selection(PrimaryExpr, String),
    Indexing(PrimaryExpr, Expression),
    Slicing(PrimaryExpr, Slice),
    TypeAssertion(PrimaryExpr, String),
    FunctionCall(PrimaryExpr, Arguments),
}

pub fn parse_primary_expr(s: &str) -> PrimaryExpr {
    unimplemented!()
}

pub enum Slice {

}
