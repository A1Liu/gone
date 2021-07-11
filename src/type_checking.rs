use crate::ast::*;
use crate::buckets::*;
use crate::parser::*;
use crate::util::*;

// Parallelization unit is file

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
    Map { key: TypeId, value: TypeId },
    Struct(&'static [StructField]),
    NamedStruct(&'static [StructField]),
    Enum(&'static [TypeId]),
    Slice(TypeId),

    Ty(TypeId),
    Range(TypeId),
    String,
    U64,

    Id(u32),
    PtrId(u32),
}

pub type TyScope<'a> = StackLL<'a, HashMap<u32, TypeId>>;

#[derive(Debug, Clone, Copy)]
pub struct Type {
    pub kind: TypeKind,
    pub decl_loc: Option<Spanned<()>>,
}

#[derive(Debug)]
pub struct GlobalTable {
    pub types: Vec<Type>,         // type-id to definition
    pub names: Vec<&'static str>, // global symbol table
}

pub struct GlobalTableBuilder {
    pub alloc: BucketListRef<'static>,
    pub table: GlobalTable,
    pub dedup_names: HashMap<&'static str, u32>,
}

impl GlobalTableBuilder {
    pub fn new(alloc: BucketListRef<'static>) -> Self {
        let none_type = Type {
            kind: TypeKind::NamedStruct(&[]),
            decl_loc: None,
        };

        let u64_type = Type {
            kind: TypeKind::U64,
            decl_loc: None,
        };

        let string_type = Type {
            kind: TypeKind::String,
            decl_loc: None,
        };

        return GlobalTableBuilder {
            alloc,
            dedup_names: map(),
            table: GlobalTable {
                types: vec![none_type, u64_type, string_type],
                names: Vec::new(),
            },
        };
    }

    pub fn translate_sym(&mut self, sym: &'static str) -> u32 {
        use std::collections::hash_map::Entry;
        match self.dedup_names.entry(sym) {
            Entry::Occupied(o) => return *o.get(),
            Entry::Vacant(v) => {
                let id = self.table.names.len() as u32;
                self.table.names.push(sym);
                v.insert(id);
                return id;
            }
        }
    }

    pub fn add_type(&mut self, ty: Type, ptr: bool) -> TypeId {
        let id = self.table.types.len() as u32;
        self.table.types.push(ty);
        return TypeId::new(id, ptr);
    }

    pub fn block(&mut self, parent: &TyScope, sym: &Symbols, block: &mut Block) -> TypeKind {
        let mut names = Vec::new();
        let mut ty_scope = parent.child(map());

        for stmt in &mut *block.stmts {
            match &mut stmt.inner {
                Stmt::Expr(e) => self.expr(&ty_scope, sym, e),
                Stmt::Decl(decls) => {
                    for decl in decls.iter_mut() {
                        let sym_str = sym.translate(decl.id.inner);
                        let id = self.translate_sym(sym_str);
                        decl.id.inner = id;
                        let ast_ty = &decl.ty.inner;
                        let ty = self.ty(&ty_scope, sym, &ast_ty.name);

                        let ty_id = self.add_type(ty, ast_ty.ptr);
                        names.push(StructField {
                            field: span(id, decl.id.begin, decl.id.end),
                            ty: ty_id,
                        });
                    }
                }
                Stmt::Type { id, def } => {
                    let sym_str = sym.translate(id.inner);
                    id.inner = self.translate_sym(sym_str);
                    let mut ty = self.ty(&ty_scope, sym, &def.name);
                    match ty.kind {
                        TypeKind::Struct(fields) => ty.kind = TypeKind::NamedStruct(fields),
                        _ => {}
                    }

                    // TODO how do we access these types?
                    let ty_id = self.add_type(ty, def.ptr);
                    if let Some(old) = ty_scope.item.insert(id.inner, ty_id) {
                        unimplemented!("two types in same scope with same name");
                    }
                }
                Stmt::Destructure(des) => {} // TODO

                // TODO eventually spawn a task on the threadpool or whatever
                Stmt::Import { path, all } => {}
                Stmt::Nop => {}
            }
        }

        let names = self.alloc.add_array(names);
        block.scope = names;
        return TypeKind::Struct(names);
    }

    pub fn expr(&mut self, scope: &TyScope, sym: &Symbols, expr: &mut Expr) {
        match expr {
            Expr::Default => {}
            Expr::Int(_) => {}
            Expr::Str(_) => {}
            Expr::Ident(id) => {
                let sym_str = sym.translate(*id);
                *id = self.translate_sym(sym_str);
            }
            Expr::True => {}
            Expr::False => {}
            Expr::NoneValue => {}

            Expr::FullRange => {}

            Expr::Bin(op, left, right) => {
                self.expr(scope, sym, &mut left.inner);
                self.expr(scope, sym, &mut right.inner);
            }

            Expr::Not(expr) => self.expr(scope, sym, &mut expr.inner),
            Expr::Ref(expr) => self.expr(scope, sym, &mut expr.inner),
            Expr::Deref(expr) => self.expr(scope, sym, &mut expr.inner),
            Expr::Field { base, field } => self.expr(scope, sym, &mut base.inner),

            Expr::Block(block) => {
                self.block(scope, sym, block);
            }
            Expr::Match(match_block) => {}
            Expr::Branch(branch) => {
                self.expr(scope, sym, &mut branch.cond.inner);
                self.expr(scope, sym, &mut branch.if_true.inner);
                self.expr(scope, sym, &mut branch.if_false.inner);
            }
            Expr::Call(callee, params) => {
                self.expr(scope, sym, &mut callee.inner);
                for param in params.iter_mut() {
                    self.expr(scope, sym, &mut param.inner);
                }
            }
        }
    }

    pub fn ty(&mut self, parent: &TyScope, sym: &Symbols, ty: &AstTypeName) -> Type {
        return Type {
            kind: TypeKind::Ty(TypeId::new_none()),
            decl_loc: None,
        };
    }
}
