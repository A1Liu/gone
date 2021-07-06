use crate::util::*;

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
#[repr(C, u8)]
pub enum Expr {
    Int(u64),
    Str(&'static str),
    Ident(u32),
    Default,
    Tuple(&'static [Spanned<Expr>]),
    True,
    False,
    NoneValue,

    // a[..] (accesses all elements)
    FullRange,

    Bin(BinOp, &'static Spanned<Expr>, &'static Spanned<Expr>),

    Not(&'static Spanned<Expr>),
    Ref(&'static Spanned<Expr>),
    Deref(&'static Spanned<Expr>),
    Field(&'static Spanned<Expr>, Spanned<u32>),

    Block(&'static [Spanned<Stmt>]),
    Match(&'static MatchBlock),
    Branch(&'static BranchExpr),
    Call(&'static Spanned<Expr>, &'static [Spanned<Expr>]),
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct BranchExpr {
    pub cond: Spanned<Expr>,
    pub if_true: Spanned<Expr>,
    pub if_false: Spanned<Expr>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Decl {
    pub id: Spanned<u32>,
    pub ty: Spanned<Type>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Destructure {
    pub pat: Spanned<Pattern>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, u8)]
pub enum Stmt {
    Expr(Expr),
    Decl(&'static [Decl]),
    Type(Type, &'static [Spanned<u32>]),
    Destructure(&'static Destructure),
    Import {
        path: &'static [Spanned<u32>],
        all: bool,
    },
    Nop,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, u8)]
pub enum TypeName {
    Struct(&'static [Decl]),
    Tuple(&'static [Spanned<Type>]),
    Enum(&'static [Spanned<Type>]),
    Slice(&'static Spanned<Type>),
    PolymorphDecl(Spanned<u32>),
    Ident(Spanned<u32>),
    U64,
    String,
    Bool,
    InferType,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Type {
    pub name: TypeName,
    pub ptr: bool,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, u8)]
pub enum PatternStructDecl {
    Name(Spanned<u32>),
    Pattern {
        id: Spanned<u32>,
        pat: Spanned<Pattern>,
    },
}

#[derive(Debug, Clone, Copy)]
#[repr(C, u8)]
pub enum PatternName {
    Struct(&'static [PatternStructDecl]),
    Tuple(&'static [Spanned<Pattern>]),
    Enum(&'static [Spanned<Pattern>]),
    Slice(&'static [Spanned<Pattern>]),
    Expr(Spanned<Expr>),
    IgnoreValue,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Pattern {
    pub name: PatternName,
    pub ptr: bool,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MatchArm {
    pub pat: Spanned<Pattern>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MatchBlock {
    pub expr: Spanned<Expr>,
    pub arms: &'static [MatchArm],
}
