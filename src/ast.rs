use crate::util::*;

#[derive(Debug)]
#[repr(u8)]
pub enum BinOp {
    Add,
    Mult,
    And,
    Xor,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    LShift,
    RShift,

    // a[0..=12] or a[..=12] (accesses values 0 and 12 and all in between)
    InclusiveRange,
    // a[0..12] or a[..12] (accesses values 0 and 11 and all in between)
    // or a[1..] (accesses values 1 until the end)
    Range,
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum Expr {
    Default,
    Int(u64),
    Str(&'static str),
    Ident(Id),
    Tuple(&'static mut [Spanned<Expr>]),
    True,
    False,
    NoneValue,

    // a[..] (accesses all elements)
    FullRange,

    Bin(
        BinOp,
        &'static mut Spanned<Expr>,
        &'static mut Spanned<Expr>,
    ),

    Not(&'static mut Spanned<Expr>),
    Ref(&'static mut Spanned<Expr>),
    Deref(&'static mut Spanned<Expr>),
    Field {
        base: &'static mut Spanned<Expr>,
        field: Spanned<Id>,
    },

    Block(&'static mut [Spanned<Stmt>]),
    Match(&'static mut MatchBlock),
    Branch(&'static mut BranchExpr),
    Call(&'static mut Spanned<Expr>, &'static mut [Spanned<Expr>]),
}

#[derive(Debug)]
#[repr(C)]
pub struct BranchExpr {
    pub cond: Spanned<Expr>,
    pub if_true: Spanned<Expr>,
    pub if_false: Spanned<Expr>,
}

#[derive(Debug)]
#[repr(C)]
pub struct Decl {
    pub id: Spanned<Id>,
    pub ty: Spanned<Type>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug)]
#[repr(C)]
pub struct Destructure {
    pub pat: Spanned<Pattern>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum Stmt {
    Expr(Expr),
    Decl(&'static mut [Decl]),
    Type {
        id: Spanned<Id>,
        def: Type,
        params: &'static mut [Spanned<Id>],
    },
    Destructure(&'static mut Destructure),
    Import {
        path: &'static mut [Spanned<Id>],
        all: bool,
    },
    Nop,
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum TypeName {
    Struct(&'static mut [Decl]),
    Tuple(&'static mut [Spanned<Type>]),
    Enum(&'static mut [Spanned<Type>]),
    Slice(&'static mut Spanned<Type>),
    PolymorphDecl(Spanned<Id>),
    Ident(Spanned<Id>),
    U64,
    String,
    Bool,
    InferType,
}

#[derive(Debug)]
#[repr(C)]
pub struct Type {
    pub name: TypeName,
    pub ptr: bool,
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum PatternStructDecl {
    Name(Spanned<Id>),
    Pattern {
        id: Spanned<Id>,
        pat: Spanned<Pattern>,
    },
}

#[derive(Debug)]
#[repr(C, u8)]
pub enum PatternName {
    Struct(&'static mut [PatternStructDecl]),
    Tuple(&'static mut [Spanned<Pattern>]),
    Enum(&'static mut [Spanned<Pattern>]),
    Slice(&'static mut [Spanned<Pattern>]),
    Expr(Spanned<Expr>),
    IgnoreValue,
}

#[derive(Debug)]
#[repr(C)]
pub struct Pattern {
    pub name: PatternName,
    pub ptr: bool,
}

#[derive(Debug)]
#[repr(C)]
pub struct MatchArm {
    pub pat: Spanned<Pattern>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug)]
#[repr(C)]
pub struct MatchBlock {
    pub expr: Spanned<Expr>,
    pub arms: &'static mut [MatchArm],
}
