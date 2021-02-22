use crate::ast::*;
use crate::buckets::*;
use crate::util::*;

pub trait Visitor {
    fn visit_stmt(&mut self, idx: StmtIdx, stmt: &mut Stmt);
    fn visit_expr(&mut self, idx: ExprIdx, expr: &mut Expr);
    fn visit_decl(&mut self, idx: DeclIdx, expr: &mut Decl);
    fn visit_type(&mut self, idx: TypeIdx, ty: &mut Type);
    fn visit_type_mod(&mut self, idx: TypeModIdx, ty: &mut TypeModifier);
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum RequestKind {
    Stmt(StmtIdx),
    Expr(ExprIdx),
    Decl(DeclIdx),
    Type(TypeIdx),
    TypeMod(TypeModIdx),
}

pub struct Request {
    kind: RequestKind,
    kids: bool,
}

impl Request {
    pub fn expr(expr: ExprIdx) -> Self {
        return Self {
            kind: RequestKind::Expr(expr),
            kids: true,
        };
    }

    pub fn stmt(stmt: StmtIdx) -> Self {
        return Self {
            kind: RequestKind::Stmt(stmt),
            kids: true,
        };
    }

    pub fn decl(decl: DeclIdx) -> Self {
        return Self {
            kind: RequestKind::Decl(decl),
            kids: true,
        };
    }

    pub fn ty(ty: TypeIdx) -> Self {
        return Self {
            kind: RequestKind::Type(ty),
            kids: true,
        };
    }

    pub fn ty_mod(ty_mod: TypeModIdx) -> Self {
        return Self {
            kind: RequestKind::TypeMod(ty_mod),
            kids: true,
        };
    }
}

pub struct GuidedVisitor<'a> {
    pub visitor: &'a mut dyn Visitor,
    pub requests: VecDeque<Request>,
}

impl<'a> Drop for GuidedVisitor<'a> {
    fn drop(&mut self) {
        unsafe { core::ptr::drop_in_place(self.visitor) };
    }
}

// yeah yeah idc i thought this was funny and thats what matters
pub struct TourGuide<'a> {
    pub buckets: BucketListFactory,
    pub ast: Ast,
    pub visitors: Vec<GuidedVisitor<'a>>,
}

impl<'a> TourGuide<'a> {
    pub fn new(ast: Ast) -> Self {
        Self {
            buckets: BucketListFactory::new(),
            ast,
            visitors: Vec::new(),
        }
    }

    pub fn greet_visitor<V: Visitor + 'a>(&mut self, visitor: V) {
        let visitor = GuidedVisitor {
            visitor: self.buckets.add_leak(visitor),
            requests: VecDeque::new(),
        };

        self.visitors.push(visitor);
    }

    // lmao what a devoted tour guide. Sacrificing themself for the sake of the tour
    pub fn give_tour(mut self) -> Ast {
        let mut to_visit = Vec::new();
        for stmt in self.ast.globals.into_iter().rev() {
            to_visit.push(Request::stmt(stmt));
        }

        while let Some(loc) = to_visit.pop() {
            match loc.kind {
                RequestKind::Expr(expr) => self.dispatch_expr(expr, loc.kids, &mut to_visit),
                RequestKind::Stmt(stmt) => self.dispatch_stmt(stmt, loc.kids, &mut to_visit),
                RequestKind::Type(ty) => self.dispatch_ty(ty, loc.kids, &mut to_visit),
                RequestKind::TypeMod(ty_mod) => {
                    self.dispatch_ty_mod(ty_mod, loc.kids, &mut to_visit)
                }
                RequestKind::Decl(decl) => self.dispatch_decl(decl, loc.kids, &mut to_visit),
            }
        }

        let new = Ast::new(self.ast.file);
        return core::mem::replace(&mut self.ast, new);
    }

    pub fn dispatch_stmt(&mut self, stmt: StmtIdx, see_children: bool, stack: &mut Vec<Request>) {
        for visitor in &mut self.visitors {
            visitor.visitor.visit_stmt(stmt, &mut self.ast[stmt]);
        }

        if !see_children {
            return;
        }

        match self.ast[stmt].kind {
            StmtKind::Ret | StmtKind::Noop | StmtKind::Break | StmtKind::Continue => return,
            _ => {}
        }

        stack.push(Request {
            kind: RequestKind::Stmt(stmt),
            kids: false,
        });

        match self.ast[stmt].kind {
            StmtKind::Expr(expr) => return self.dispatch_expr(expr, true, stack),
            StmtKind::Decl(decl) => return self.dispatch_decl(decl, true, stack),
            StmtKind::RetVal(val) => stack.push(Request::expr(val)),

            StmtKind::For { iter, source, body } => {
                stack.push(Request::expr(iter));
                stack.push(Request::expr(source));
                stack.push(Request::stmt(body));
            }

            StmtKind::While { condition, body } => {
                stack.push(Request::expr(condition));
                stack.push(Request::stmt(body));
            }

            StmtKind::Branch {
                if_cond,
                if_body,
                else_body,
            } => {
                if let Some(else_body) = else_body {
                    stack.push(Request::stmt(else_body));
                }

                stack.push(Request::stmt(if_body));
                stack.push(Request::expr(if_cond));
            }
            StmtKind::Ret | StmtKind::Noop | StmtKind::Break | StmtKind::Continue => unreachable!(),
        }
    }

    pub fn dispatch_expr(&mut self, expr: ExprIdx, see_children: bool, stack: &mut Vec<Request>) {
        for visitor in &mut self.visitors {
            visitor.visitor.visit_expr(expr, &mut self.ast[expr]);
        }

        if !see_children {
            return;
        }

        match self.ast[expr].kind {
            ExprKind::Null | ExprKind::Ux(_) => return,
            ExprKind::StringLit(_) | ExprKind::Ident(_) => return,
            ExprKind::Struct(stmts) if stmts.start == stmts.end => return,
            ExprKind::Block { stmts } if stmts.start == stmts.end => return,
            ExprKind::List { values } if values.start == values.end => return,
            _ => {}
        }

        stack.push(Request {
            kind: RequestKind::Expr(expr),
            kids: false,
        });

        match self.ast[expr].kind {
            ExprKind::New(ty) => stack.push(Request::ty(ty)),
            ExprKind::Function { params, body } => {
                for stmt in body.rev() {
                    stack.push(Request::stmt(stmt));
                }
                for param in params.rev() {
                    stack.push(Request::decl(param));
                }
            }
            ExprKind::Block { stmts } => {
                for stmt in stmts.rev() {
                    stack.push(Request::stmt(stmt));
                }
            }
            ExprKind::Struct(stmts) => {
                for stmt in stmts.rev() {
                    stack.push(Request::stmt(stmt));
                }
            }
            ExprKind::List { values } => {
                for value in values.rev() {
                    stack.push(Request::expr(value));
                }
            }

            ExprKind::UnitStruct(ty) => stack.push(Request::ty(ty)),
            ExprKind::UnaryOp(op, expr) => stack.push(Request::expr(expr)),
            ExprKind::BinOp(op, a, b) => {
                stack.push(Request::expr(b));
                stack.push(Request::expr(a));
            }
            ExprKind::Range(begin, end) => {
                if let Some(end) = end {
                    stack.push(Request::expr(end));
                }
                if let Some(begin) = begin {
                    stack.push(Request::expr(begin));
                }
            }
            ExprKind::Member { member, base } => stack.push(Request::expr(base)),
            ExprKind::Call { function, params } => {
                for param in params.rev() {
                    stack.push(Request::expr(param));
                }
                stack.push(Request::expr(function));
            }
            ExprKind::Ternary {
                condition,
                if_true,
                if_false,
            } => {
                stack.push(Request::expr(if_false));
                stack.push(Request::expr(if_true));
                stack.push(Request::expr(condition));
            }
            ExprKind::Assign(target, value) => {
                stack.push(Request::expr(value));
                stack.push(Request::expr(target));
            }
            ExprKind::MutAssign { op, target, value } => {
                stack.push(Request::expr(value));
                stack.push(Request::expr(target));
            }
            ExprKind::Cast { ty, expr } => {
                stack.push(Request::expr(expr));
                stack.push(Request::ty(ty));
            }
            ExprKind::Null | ExprKind::Ux(_) | ExprKind::StringLit(_) | ExprKind::Ident(_) => {
                unreachable!()
            }
        }
    }

    pub fn dispatch_decl(&mut self, decl: DeclIdx, see_children: bool, stack: &mut Vec<Request>) {
        for visitor in &mut self.visitors {
            visitor.visitor.visit_decl(decl, &mut self.ast[decl]);
        }

        if !see_children {
            return;
        }

        stack.push(Request {
            kind: RequestKind::Decl(decl),
            kids: false,
        });

        if let Some(expr) = self.ast[decl].expr {
            stack.push(Request::expr(expr));
        }

        stack.push(Request::ty(self.ast[decl].ty));
    }

    pub fn dispatch_ty(&mut self, ty: TypeIdx, see_children: bool, stack: &mut Vec<Request>) {
        for visitor in &mut self.visitors {
            visitor.visitor.visit_type(ty, &mut self.ast[ty]);
        }

        let mods = self.ast[ty].modifiers;
        if !see_children || mods.start == mods.end {
            return;
        }

        stack.push(Request {
            kind: RequestKind::Type(ty),
            kids: false,
        });

        for ty_mod in self.ast[ty].modifiers.into_iter().rev() {
            stack.push(Request::ty_mod(ty_mod));
        }
    }

    pub fn dispatch_ty_mod(&mut self, ty_mod: TypeModIdx, kids: bool, stack: &mut Vec<Request>) {
        for visitor in &mut self.visitors {
            #[rustfmt::skip]
            visitor.visitor.visit_type_mod(ty_mod, &mut self.ast[ty_mod]);
        }

        if !kids {
            return;
        }

        let to_push = match self.ast[ty_mod] {
            TypeModifier::Array(arr) => arr,
            _ => return,
        };

        stack.push(Request {
            kind: RequestKind::TypeMod(ty_mod),
            kids: false,
        });

        stack.push(Request::expr(to_push));
    }
}
