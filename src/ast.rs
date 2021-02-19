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
pub enum TypeModifier {
    Pointer,
    Array(u64),
    VarArray,
    Slice,
    Varargs,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeBase<'a> {
    Any,
    S64,
    U64,
    Named(u32),
    Function {
        ret: &'a Type<'a>,
        params: &'a [Type<'a>],
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Type<'a> {
    pub modifiers: &'a [TypeModifier],
    pub base: TypeBase<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Decl<'a> {
    pub idents: &'a [u32],
    pub ty: Option<Type<'a>>,
    pub expr: Option<&'a Expr<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'a> {
    Ux(u64),
    // Sx(i64),
    StringLit(&'a str),
    Ident(u32),
    Function {
        params: &'a [(Decl<'a>, CodeLoc)],
        body: &'a Expr<'a>,
    },
    Block(&'a [Statement<'a>]),
    BinOp(BinOp, &'a Expr<'a>, &'a Expr<'a>),
    Range(Option<&'a Expr<'a>>, Option<&'a Expr<'a>>),
    List {
        ty: Option<Type<'a>>,
        values: &'a [Expr<'a>],
    },
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
    Cast {
        ty: Option<Type<'a>>,
        expr: &'a Expr<'a>,
    },
    Assign(&'a Expr<'a>, &'a Expr<'a>),
    MutAssign {
        op: BinOp,
        target: &'a Expr<'a>,
        value: &'a Expr<'a>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum StatementKind<'a> {
    Noop,
    Expr(ExprKind<'a>),
    Decl(Decl<'a>),
    Ret,
    RetVal(Expr<'a>),
    Branch {
        if_cond: Expr<'a>,
        if_body: &'a Statement<'a>,
        else_body: Option<&'a Statement<'a>>,
    },
    For {
        iter: Expr<'a>,
        source: Expr<'a>,
        body: &'a Statement<'a>,
    },
    While {
        condition: Option<Expr<'a>>,
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
