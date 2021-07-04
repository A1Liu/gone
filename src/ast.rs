#[derive(Clone, Copy)]
#[repr(C)]
pub struct Spanned<T: Copy> {
    // TODO these could be u32's probably
    // TODO should this be a generic wrapper or just put as fields?
    pub inner: T,
    pub begin: usize,
    pub end: usize,
}

pub fn span<T: Copy>(inner: T, begin: usize, end: usize) -> Spanned<T> {
    return Spanned {
        inner,
        begin: begin,
        end: end,
    };
}

#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum Expr {
    Int(u64),
    Str(&'static str),
    Ident(u32),

    Add(&'static Spanned<Expr>, &'static Spanned<Expr>),
    And(&'static Spanned<Expr>, &'static Spanned<Expr>),
    Xor(&'static Spanned<Expr>, &'static Spanned<Expr>),
    Or(&'static Spanned<Expr>, &'static Spanned<Expr>),
    Eq(&'static Spanned<Expr>, &'static Spanned<Expr>),
    Neq(&'static Spanned<Expr>, &'static Spanned<Expr>),
    Lt(&'static Spanned<Expr>, &'static Spanned<Expr>),
    Gt(&'static Spanned<Expr>, &'static Spanned<Expr>),
    Leq(&'static Spanned<Expr>, &'static Spanned<Expr>),
    Geq(&'static Spanned<Expr>, &'static Spanned<Expr>),
    LShift(&'static Spanned<Expr>, &'static Spanned<Expr>),
    RShift(&'static Spanned<Expr>, &'static Spanned<Expr>),

    Ref(&'static Spanned<Expr>),
    Deref(&'static Spanned<Expr>),
    Field(&'static Spanned<Expr>, Spanned<u32>),

    Block(&'static [Spanned<Stmt>]),
    Branch(&'static BranchExpr),
    Call(&'static Spanned<Expr>, &'static [Spanned<Expr>]),
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct BranchExpr {
    pub cond: Spanned<Expr>,
    pub if_true: Spanned<Expr>,
    pub if_false: Spanned<Expr>,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct BranchStmt {
    pub cond: Spanned<Expr>,
    pub if_true: Spanned<Stmt>,
    pub if_false: Spanned<Stmt>,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Decl {
    pub id: Spanned<u32>,
    pub ty: Spanned<Type>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum Stmt {
    Expr(Expr),
    Block(&'static [Spanned<Stmt>]),
    Branch(&'static BranchStmt),
    Decl(&'static [Decl]),
    Nop,
}

#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum TypeName {
    Enum(&'static [Spanned<Type>]),
    Slice(&'static Spanned<Type>),
    Ident(Spanned<u32>),
    U64,
    String,
    None,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Type {
    pub name: TypeName,
    pub pointer: bool,
}
