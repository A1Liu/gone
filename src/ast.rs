use crate::util::*;
use core::num::NonZeroU32;

#[derive(Debug, Clone, Copy)]
pub struct StrIdx(NonZeroU32);
#[derive(Debug, Clone, Copy)]
pub struct ParamIdx(NonZeroU32);
#[derive(Debug, Clone, Copy)]
pub struct IdentIdx(NonZeroU32);
#[derive(Debug, Clone, Copy)]
pub struct ExprIdx(NonZeroU32);
#[derive(Debug, Clone, Copy)]
pub struct StmtIdx(NonZeroU32);
#[derive(Debug, Clone, Copy)]
pub struct TypeModIdx(NonZeroU32);
#[derive(Debug, Clone, Copy)]
pub struct TypeIdx(NonZeroU32);

static_assertions::assert_eq_size!(TypeModIdx, Option<TypeModIdx>);

pub struct Ast {
    pub file: u32,
    pub strings: StringArray<()>,
    pub params: Vec<(Decl, CodeLoc)>,
    pub idents: Vec<u32>,
    pub exprs: Vec<Expr>,
    pub stmts: Vec<Stmt>,
    pub ty_mods: Vec<TypeModifier>,
    pub tys: Vec<Type>,
    pub globals: Range<StmtIdx>,
}

fn w(i: u32) -> NonZeroU32 {
    return NonZeroU32::new(i).unwrap();
}

impl Ast {
    pub fn new(file: u32) -> Self {
        let idx = StmtIdx(w(!0));

        Self {
            file,
            strings: StringArray::new(),
            params: Vec::new(),
            idents: Vec::new(),
            exprs: Vec::new(),
            stmts: Vec::new(),
            ty_mods: Vec::new(),
            tys: Vec::new(),
            globals: r(idx, idx),
        }
    }

    pub fn add_str(&mut self, string: String) -> StrIdx {
        let idx = w(!self.strings.len() as u32);
        self.strings.push((), &string);
        return StrIdx(idx);
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprIdx {
        let idx = w(!self.exprs.len() as u32);
        self.exprs.push(expr);
        return ExprIdx(idx);
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> StmtIdx {
        let idx = w(!self.stmts.len() as u32);
        self.stmts.push(stmt);
        return StmtIdx(idx);
    }

    pub fn add_ty(&mut self, ty: Type) -> TypeIdx {
        let idx = w(!self.tys.len() as u32);
        self.tys.push(ty);
        return TypeIdx(idx);
    }

    pub fn add_stmts(&mut self, mut stmts: Vec<Stmt>) -> Range<StmtIdx> {
        let begin = StmtIdx(w(!self.stmts.len() as u32));
        self.stmts.append(&mut stmts);
        return r(begin, StmtIdx(w(!self.stmts.len() as u32)));
    }

    pub fn add_exprs(&mut self, mut exprs: Vec<Expr>) -> Range<ExprIdx> {
        let begin = ExprIdx(w(!self.exprs.len() as u32));
        self.exprs.append(&mut exprs);
        return r(begin, ExprIdx(w(!self.exprs.len() as u32)));
    }

    pub fn add_idents(&mut self, mut idents: Vec<u32>) -> Range<IdentIdx> {
        let begin = IdentIdx(w(!self.idents.len() as u32));
        self.idents.append(&mut idents);
        return r(begin, IdentIdx(w(!self.idents.len() as u32)));
    }

    pub fn add_ty_mods(&mut self, mut ty_mods: Vec<TypeModifier>) -> Range<TypeModIdx> {
        let begin = TypeModIdx(w(!self.ty_mods.len() as u32));
        self.ty_mods.append(&mut ty_mods);
        return r(begin, TypeModIdx(w(!self.ty_mods.len() as u32)));
    }

    pub fn add_params(&mut self, mut params: Vec<(Decl, CodeLoc)>) -> Range<ParamIdx> {
        let begin = ParamIdx(w(!self.params.len() as u32));
        self.params.append(&mut params);
        return r(begin, ParamIdx(w(!self.params.len() as u32)));
    }
}

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
    Array(ExprIdx),
    VarArray,
    Slice,
    Varargs,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeBase {
    Any,
    String,
    S64,
    U64,
    Named(u32),
    Function {
        ret: TypeIdx,
        params: Range<TypeIdx>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Type {
    pub modifiers: Range<TypeModIdx>,
    pub base: TypeBase,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Decl {
    pub idents: Range<IdentIdx>,
    pub ty: Option<TypeIdx>,
    pub expr: Option<ExprIdx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind {
    Null,
    Ux(u64),
    // Sx(i64),
    StringLit(StrIdx),
    Ident(u32),
    New(TypeIdx),
    Function {
        params: Range<ParamIdx>,
        body: ExprIdx,
    },
    Block(Range<StmtIdx>),
    Struct(Range<StmtIdx>),
    UnitStruct(TypeIdx),
    BinOp(BinOp, ExprIdx, ExprIdx),
    Range(Option<ExprIdx>, Option<ExprIdx>),
    List {
        ty: Option<TypeIdx>,
        values: Range<ExprIdx>,
    },
    Member {
        member: u32,
        base: ExprIdx,
    },
    UnaryOp(UnaryOp, ExprIdx),
    Call {
        function: ExprIdx,
        params: Range<ExprIdx>,
    },
    Ternary {
        condition: ExprIdx,
        if_true: ExprIdx,
        if_false: ExprIdx,
    },
    Cast {
        ty: Option<TypeIdx>,
        expr: ExprIdx,
    },
    Assign(ExprIdx, ExprIdx),
    MutAssign {
        op: BinOp,
        target: ExprIdx,
        value: ExprIdx,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<TypeIdx>,
    pub loc: CodeLoc,
}

pub fn e(kind: ExprKind, loc: CodeLoc) -> Expr {
    return Expr {
        kind,
        ty: None,
        loc,
    };
}

#[derive(Debug, Clone, Copy)]
pub enum StmtKind {
    Noop,
    Expr(ExprKind),
    Decl(Decl),
    Ret,
    RetVal(ExprIdx),
    Branch {
        if_cond: ExprIdx,
        if_body: StmtIdx,
        else_body: Option<StmtIdx>,
    },
    For {
        iter: ExprIdx,
        source: ExprIdx,
        body: StmtIdx,
    },
    While {
        condition: Option<ExprIdx>,
        body: StmtIdx,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub struct Stmt {
    pub kind: StmtKind,
    pub loc: CodeLoc,
}
