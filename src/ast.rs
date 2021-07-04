use std::fmt;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Spanned<T> {
    // TODO these could be u32's probably
    // TODO should this be a generic wrapper or just put as fields?
    pub inner: T,
    pub begin: usize,
    pub end: usize,
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        return self.inner.fmt(fmt);
    }
}

pub fn span<T>(inner: T, begin: usize, end: usize) -> Spanned<T> {
    return Spanned {
        inner,
        begin: begin,
        end: end,
    };
}

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
    Range,
    InclusiveRange,
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

    Bin(BinOp, &'static Spanned<Expr>, &'static Spanned<Expr>),

    Ref(&'static Spanned<Expr>),
    Deref(&'static Spanned<Expr>),
    Field(&'static Spanned<Expr>, Spanned<u32>),
    Splat(&'static Spanned<Expr>),

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
    None,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Type {
    pub name: TypeName,
    pub pointer: bool,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PatternStructDecl {
    pub id: Spanned<u32>,
    pub pat: Spanned<Pattern>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, u8)]
pub enum PatternName {
    Struct(&'static [PatternStructDecl]),
    Tuple(&'static [Spanned<Pattern>]),
    Enum(&'static [Spanned<Pattern>]),
    Slice(&'static [Spanned<Pattern>]),
    Expr(Spanned<Expr>),
    None,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Pattern {
    pub name: PatternName,
    pub pointer: bool,
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
