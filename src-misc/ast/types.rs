use super::*;

/// An array type.
///
/// ## Grammar
///
/// ```ignore
/// ArrayType   = "[" ArrayLength "]" ElementType .
/// ArrayLength = Expression .
/// ElementType = Type .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub len: Expr,
    pub element_type: Type,
}

/// A struct type.
///
/// ## Grammar
///
/// ```ignore
/// StructType     = "struct" "{" { FieldDecl ";" } "}" .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub field_decls: Vec<FieldDecl>,
}


/// An inner field decl.
///
/// ## Grammar
///
/// ```ignore
/// FieldDecl      = (IdentifierList Type | AnonymousField) [ Tag ] .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldDecl {
    pub inner: InnerFieldDecl,
    pub tag: Option<Vec<u8>>, // Go string literal; TODO proper wrapper type
}

/// An InnerFieldDecl.
///
/// ## Grammar
///
/// ```ignore
/// AnonymousField = [ "*" ] TypeName .
/// Tag            = string_lit .
/// TypeName  = identifier | QualifiedIdent .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InnerFieldDecl {
    Named {
        idents: Vec<Ident>,
        typ: Type,
    },
    Anonymous {
        is_ptr: bool,
        type_name: MaybeQualifiedIdent,
    },
}


/// A type which is a pointer to a base type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerType(pub Type);



/// A function type denotes the set of all functions with the same parameter and result types. The
/// value of an uninitialized variable of function type is nil.
///
/// ## Grammar
///
/// ```ignore
/// FunctionType   = "func" Signature .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub signature: FuncSignature,
}


/// An interface type specifies a method set called its interface.
///
/// ## Grammar
///
/// ```ignore
/// InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceType {
    pub specs: Vec<MethodSpec>,
}

/// An interface method spec.
///
/// ## Grammar
///
/// ```ignore
/// MethodSpec         = MethodName Signature | InterfaceTypeName .
/// MethodName         = identifier .
/// InterfaceTypeName  = TypeName .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodSpec {
    pub name: Ident,
    pub method: InnerMethodSpec,
}

// XXX: naming things is hard
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InnerMethodSpec {
    Signature(FuncSignature),
    InterfaceName(TypeName),
}

/// A slice type denotes the set of all slices of arrays of its element type.
///
/// ## Grammar
///
/// ```ignore
/// SliceType = "[" "]" ElementType .
/// ElementType = Type .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SliceType {
    pub element_type: Type,
}

/// A map is an unordered group of elements of one type, called the element type, indexed by a set
/// of unique keys of another type, called the key type.
/// ## Grammar
///
/// ```ignore
/// MapType     = "map" "[" KeyType "]" ElementType .
/// KeyType     = Type .
/// ElementType = Type .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapType {
    pub key_type: Type,
    pub element_type: Type,
}

/// A channel provides a mechanism for concurrently executing functions to communicate by sending
/// and receiving values of a specified element type.
///
/// ## Grammar
///
/// ```ignore
/// ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
/// ElementType = Type .
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChanType {
    pub element_type: Type,
    pub direction: ChanDirection,
}


/// The optional <- operator specifies the **channel direction**, send or receive. If no direction
/// is given, the channel is bidirectional.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChanDirection {
    /// No arrow.
    Bidirectional,
    /// Arrow on the right: `chan<-`
    Send,
    /// Arrow on the left: `<-chan`
    Receive,
}
