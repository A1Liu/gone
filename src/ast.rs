use crate::buckets::*;
use crate::util::*;
use std::collections::HashMap;

#[derive(Debug)]
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
pub enum Expr {
    Default,
    Int(u64),
    Str(&'static str),
    Ident(u32),
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
        field: Spanned<u32>,
    },

    Block(Block),
    Match(&'static mut MatchBlock),
    Branch(&'static mut BranchExpr),
    Call(&'static mut Spanned<Expr>, &'static mut [Spanned<Expr>]),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: &'static mut [Spanned<Stmt>],
    pub scope: &'static [StructField],
    pub type_id: TypeId,
}

impl Block {
    pub fn new(a: &impl Allocator<'static>, stmts: Vec<Spanned<Stmt>>) -> Self {
        Block {
            stmts: a.add_array(stmts),
            scope: &[],
            type_id: TypeId::new_none(),
        }
    }
}

#[derive(Debug)]
pub struct BranchExpr {
    pub cond: Spanned<Expr>,
    pub if_true: Spanned<Expr>,
    pub if_false: Spanned<Expr>,
}

#[derive(Debug)]
pub struct Decl {
    pub id: Spanned<u32>,
    pub ty: Spanned<AstType>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug)]
pub struct Destructure {
    pub pat: Spanned<Pattern>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Decl(&'static mut [Decl]),
    Type {
        id: Spanned<u32>,
        def: AstType,
    },
    Destructure(&'static mut Destructure),
    Import {
        path: &'static mut [Spanned<u32>],
        all: bool,
    },
    Nop,
}

#[derive(Debug)]
pub enum AstTypeName {
    Struct(Block),
    Enum(&'static [Spanned<AstType>]),
    Slice(&'static Spanned<AstType>),
    Ident(Spanned<u32>),
    NoneType,
    U64,
    String,
    Bool,
    InferType,
    Checked(TypeId),
}

#[derive(Debug)]
pub struct AstType {
    pub name: AstTypeName,
    pub ptr: bool,
}

#[derive(Debug)]
pub enum PatternStructDecl {
    Name(Spanned<u32>),
    Pattern {
        id: Spanned<u32>,
        pat: Spanned<Pattern>,
    },
}

#[derive(Debug)]
pub enum PatternName {
    Struct(&'static mut [PatternStructDecl]),
    Enum(&'static mut [Spanned<Pattern>]),
    Slice(&'static mut [Spanned<Pattern>]),
    Expr(Spanned<Expr>),
    IgnoreValue,
}

#[derive(Debug)]
pub struct Pattern {
    pub name: PatternName,
    pub ptr: bool,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pat: Spanned<Pattern>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug)]
pub struct MatchBlock {
    pub expr: Spanned<Expr>,
    pub arms: &'static mut [MatchArm],
}

// TYPE CHECKING STUFF

#[derive(Debug)]
pub struct StructField {
    pub field: Spanned<u32>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeId(u32);

impl TypeId {
    const PTR_FLAG: u32 = 1 << 31;
    const RESERVED: u32 = Self::PTR_FLAG;

    pub fn new(id: u32, ptr: bool) -> Self {
        assert!((Self::RESERVED & id) == 0, "id was too large {}", id);

        let mut flags = 0;
        if ptr {
            flags |= Self::PTR_FLAG;
        }

        return Self(id | flags);
    }

    pub fn is_ptr(&self) -> bool {
        return (Self::PTR_FLAG & self.0) != 0;
    }

    pub fn deref(self) -> Option<Self> {
        if self.is_ptr() {
            return Some(Self(self.idx()));
        }

        return None;
    }

    pub fn addr_of(self) -> Option<Self> {
        if self.is_ptr() {
            return None;
        }

        return Some(Self(self.0 | Self::PTR_FLAG));
    }

    pub fn idx(&self) -> u32 {
        return self.0 & !Self::PTR_FLAG;
    }

    pub fn new_none() -> Self {
        return Self(0);
    }

    pub fn new_u64() -> Self {
        return Self(1);
    }

    pub fn new_string() -> Self {
        return Self(2);
    }
}
