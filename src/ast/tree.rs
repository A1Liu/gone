use crate::filedb::*;
use crate::util::*;
use core::num::{NonZeroI32, NonZeroU32};
use core::ops::{Index, IndexMut};

fn w(i: u32) -> NonZeroU32 {
    return NonZeroU32::new(i).unwrap();
}

macro_rules! define_idx {
    ($name:ident, $target:ident, $collection:ident) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub struct $name(NonZeroU32);

        impl $name {
            pub const unsafe fn new_unchecked(idx: u32) -> Self {
                return $name(NonZeroU32::new_unchecked(!idx));
            }

            pub fn illegal() -> Self {
                return $name(w(1));
            }

            pub fn add(self, op: u32) -> Self {
                let idx = !self.0.get() + op;
                return Self(w(!idx));
            }

            pub fn sub(self, op: u32) -> Self {
                let idx = !self.0.get() - op;
                return Self(w(!idx));
            }
        }

        impl Range<$name> {
            pub fn len(self) -> u32 {
                return !self.end.0.get() - !self.start.0.get();
            }
        }

        impl Iterator for Range<$name> {
            type Item = $name;

            fn next(&mut self) -> Option<$name> {
                if self.start == self.end {
                    return None;
                }

                let value = self.start;
                self.start = self.start.add(1);
                return Some(value);
            }
        }

        impl DoubleEndedIterator for Range<$name> {
            fn next_back(&mut self) -> Option<$name> {
                if self.start == self.end {
                    return None;
                }

                self.start = self.start.sub(1);
                return Some(self.start);
            }
        }

        impl Index<$name> for Ast {
            type Output = $target;

            fn index(&self, idx: $name) -> &$target {
                return &self.$collection[!idx.0.get() as usize];
            }
        }

        impl IndexMut<$name> for Ast {
            fn index_mut(&mut self, idx: $name) -> &mut $target {
                return &mut self.$collection[!idx.0.get() as usize];
            }
        }

        impl Index<Range<$name>> for Ast {
            type Output = [$target];

            fn index(&self, idx: Range<$name>) -> &[$target] {
                return &self.$collection[idx.map(|i| !i.0.get() as usize).norm()];
            }
        }

        impl IndexMut<Range<$name>> for Ast {
            fn index_mut(&mut self, idx: Range<$name>) -> &mut [$target] {
                return &mut self.$collection[idx.map(|i| !i.0.get() as usize).norm()];
            }
        }
    };
}

pub struct Ast {
    pub strings: StringArray<()>,
    pub decls: Vec<Decl>,
    pub exprs: Vec<Expr>,
    pub stmts: Vec<Stmt>,
    pub ty_mods: Vec<TypeModifier>,
    pub tys: Vec<Type>,
    pub globals: Range<StmtIdx>,
    pub file: u32,
}

define_idx!(DeclIdx, Decl, decls);
define_idx!(TypeIdx, Type, tys);
define_idx!(TypeModIdx, TypeModifier, ty_mods);
define_idx!(StmtIdx, Stmt, stmts);
define_idx!(ExprIdx, Expr, exprs);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct TernaryIdx(ExprIdx);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct PairIdx(ExprIdx);

impl TernaryIdx {
    #[inline]
    pub fn condition(self) -> ExprIdx {
        return self.0;
    }

    #[inline]
    pub fn if_true(self) -> ExprIdx {
        return self.0.add(1);
    }

    #[inline]
    pub fn if_false(self) -> ExprIdx {
        return self.0.add(2);
    }
}

impl PairIdx {
    #[inline]
    pub fn get_0(self) -> ExprIdx {
        return self.0;
    }

    #[inline]
    pub fn get_1(self) -> ExprIdx {
        return self.0.add(1);
    }
}

pub const INFER_TYPE: Type = Type {
    modifiers: r!(unsafe { TypeModIdx::new_unchecked(0) }, unsafe {
        TypeModIdx::new_unchecked(0)
    }),
    base: TypeBase::Named(BuiltinSymbol::Underscore as u32),
};

pub const INFER_TYPE_IDX: TypeIdx = unsafe { TypeIdx::new_unchecked(0) };

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StrIdx(NonZeroU32);

impl Index<StrIdx> for Ast {
    type Output = str;

    fn index(&self, idx: StrIdx) -> &str {
        let TS(tag, data) = &self.strings[!idx.0.get() as usize];
        return data;
    }
}

impl Ast {
    pub fn new(file: u32) -> Self {
        let idx = StmtIdx(w(!0));

        Self {
            file,
            strings: StringArray::new(),
            decls: Vec::new(),
            exprs: Vec::new(),
            stmts: Vec::new(),
            ty_mods: Vec::new(),
            tys: vec![INFER_TYPE],
            globals: r!(idx, idx),
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

    pub fn add_ternary(&mut self, condition: Expr, if_true: Expr, if_false: Expr) -> TernaryIdx {
        let idx = w(!self.exprs.len() as u32);
        self.exprs.push(condition);
        self.exprs.push(if_true);
        self.exprs.push(if_false);
        return TernaryIdx(ExprIdx(idx));
    }

    pub fn add_pair(&mut self, left: Expr, right: Expr) -> PairIdx {
        let idx = w(!self.exprs.len() as u32);
        self.exprs.push(left);
        self.exprs.push(right);
        return PairIdx(ExprIdx(idx));
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
        return r!(begin, StmtIdx(w(!self.stmts.len() as u32)));
    }

    pub fn add_exprs(&mut self, mut exprs: Vec<Expr>) -> Range<ExprIdx> {
        let begin = ExprIdx(w(!self.exprs.len() as u32));
        self.exprs.append(&mut exprs);
        return r!(begin, ExprIdx(w(!self.exprs.len() as u32)));
    }

    pub fn add_ty_mods(&mut self, mut ty_mods: Vec<TypeModifier>) -> Range<TypeModIdx> {
        let begin = TypeModIdx(w(!self.ty_mods.len() as u32));
        self.ty_mods.append(&mut ty_mods);
        return r!(begin, TypeModIdx(w(!self.ty_mods.len() as u32)));
    }

    pub fn add_tys(&mut self, mut tys: Vec<Type>) -> Range<TypeIdx> {
        let begin = TypeIdx(w(!self.tys.len() as u32));
        self.tys.append(&mut tys);
        return r!(begin, TypeIdx(w(!self.tys.len() as u32)));
    }

    pub fn add_decls(&mut self, mut decls: Vec<Decl>) -> Range<DeclIdx> {
        let begin = DeclIdx(w(!self.decls.len() as u32));
        self.decls.append(&mut decls);
        return r!(begin, DeclIdx(w(!self.decls.len() as u32)));
    }

    pub fn e(&mut self, kind: ExprKind, loc: CodeLoc) -> Expr {
        return Expr {
            kind,
            ty: self.add_ty(INFER_TYPE),
            loc,
        };
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
    Type,
    S64,
    U64,
    Ux {
        max: NonZeroU32, // if it needs U64 of space to represent itself, its a U64
    },
    Sx {
        min: NonZeroI32,
        max: NonZeroI32,
    },
    EqTo(TypeIdx),
    Named(u32),
    Function {
        ret_and_params: Range<TypeIdx>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Type {
    pub modifiers: Range<TypeModIdx>,
    pub base: TypeBase,
}

#[derive(Debug, Clone, Copy)]
pub struct Decl {
    pub ident: u32,
    pub ty: TypeIdx,
    pub expr: Option<ExprIdx>,
    pub loc: CodeLoc,
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
        params: DeclIdx,
        params_len: u16,
        body: Range<StmtIdx>,
    },
    Block {
        stmts: Range<StmtIdx>,
    },
    Struct(Range<StmtIdx>),
    UnitStruct(TypeIdx),
    UnaryOp(UnaryOp, ExprIdx),
    BinOp(BinOp, PairIdx),
    Range(ExprIdx, ExprIdx),
    List {
        values: Range<ExprIdx>,
    },
    Member {
        base: ExprIdx,
        member: u32,
    },
    Call {
        func_and_params: Range<ExprIdx>,
    },
    Ternary(TernaryIdx),
    Cast {
        ty: TypeIdx,
        expr: ExprIdx,
    },
    Assign(PairIdx),
    MutAssign(BinOp, PairIdx),
}

#[derive(Debug, Clone, Copy)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: TypeIdx,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum StmtKind {
    Noop,
    Expr(ExprIdx),
    Decl(DeclIdx),
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
        condition: ExprIdx,
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
