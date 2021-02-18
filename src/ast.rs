use crate::util::*;

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Index,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    LShift,
    RShift,
    BitAnd,
    BitXor,
    BitOr,
    BoolAnd,
    BoolOr,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum UnaryOp {
    Neg,
    BoolNot,
    BitNot,
    PostIncr,
    PostDecr,
    PreIncr,
    PreDecr,
    Deref,
    Ref,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'a> {
    Number(u64),
    // DoubleLit(f64),
    StringLit(&'a str),
    Ident(u32),
    BinOp(BinOp, &'a Expr<'a>, &'a Expr<'a>),
    Member {
        member: u32,
        base: &'a Expr<'a>,
    },
    UnaryOp(UnaryOp, &'a Expr<'a>),
    Call {
        function: &'a Expr<'a>,
        params: &'a [Expr<'a>],
    },
    Ternary {
        condition: &'a Expr<'a>,
        if_true: &'a Expr<'a>,
        if_false: &'a Expr<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Decl<'a> {
    pub ident: u32,
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct Block<'a> {
    pub stmts: &'a [Statement<'a>],
}

#[derive(Debug, Clone, Copy)]
pub enum StatementKind<'a> {
    Expr(Expr<'a>),
    Ret,
    RetVal(Expr<'a>),
    Branch {
        if_cond: Expr<'a>,
        if_body: &'a Statement<'a>,
        else_body: Option<&'a Statement<'a>>,
    },
    Block(Block<'a>),
    ForDecl {
        decl: Decl<'a>,
        condition: Option<Expr<'a>>,
        post_expr: Option<Expr<'a>>,
        body: &'a Statement<'a>,
    },
    While {
        condition: Expr<'a>,
        body: &'a Statement<'a>,
    },
    DoWhile {
        condition: Expr<'a>,
        body: &'a Statement<'a>,
    },
    Switch {
        expr: Expr<'a>,
        body: &'a Statement<'a>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub struct Statement<'a> {
    pub kind: StatementKind<'a>,
    pub loc: CodeLoc,
}
