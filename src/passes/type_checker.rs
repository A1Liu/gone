use crate::ast::*;
use crate::util::*;

// DISJOINT SET UNION FIND ALGORITHM LETS GOOOOOOOO
// (for type inference in one pass)
// Idea: Most types are declared; the few types that arent are almost always inferred
// by just copying the id of the type. However, we have one case that's actually
// complicated, which is typechecking variables that have been assigned integer literals.
// To simplify this case, we require that only integer literals of the same type
// can be added together. This allows us to us a DISJOINT SET UNION FIND to fill in types

pub enum ScopeType {
    // (a: string) => { /* this kind of scope */ }
    // continue and break are disabled
    Function { ret_ty: TypeIdx },

    // The global scope.
    // continue, break, return are disabled
    Global,

    // a := { /* this kind of scope */ }
    // continue/break affect enclosing loop
    Value,

    // { /* this kind of scope */ }
    // continue/break affect enclosing loop
    Block,
}

pub struct Scope {
    pub decls: HashMap<u32, TypeIdx>,
    pub ty: ScopeType,
}

type Env<'a> = StackLL<'a, Scope>;

macro_rules! error {
    ($info:expr, $loc:expr, $file:expr, $( $tt:tt ),* ) => {{
        Error {
            info: $info,
            message: Some(format!( $( $tt )* )),
            loc: $loc,
            file: $file,
            #[cfg(debug_assertions)]
            compiler_loc: here!(),
        }
    }};
}

pub struct Checker {
    pub ty_roots: HashMap<TypeIdx, ()>,
    pub ast: Ast,
}

impl Checker {
    // just does scope checking, without any of the fancy language features associated with scopes
    pub fn check_scope(&mut self, env: &mut Env, stmts: Range<StmtIdx>) -> Result<(), Error> {
        for stmt in stmts {
            self.check_stmt(env, stmt)?;
        }

        return Ok(());
    }

    pub fn check_stmt(&mut self, env: &mut Env, stmt: StmtIdx) -> Result<(), Error> {
        match self.ast[stmt].kind {
            StmtKind::Noop => {}
            StmtKind::Expr(expr) => core::mem::drop(self.check_expr(env, expr)?),
            StmtKind::Decl(decl) => self.check_decl(env, decl)?,
            _ => unimplemented!(),
        }

        return Ok(());
    }

    pub fn check_decl(&mut self, env: &mut Env, decl: DeclIdx) -> Result<(), Error> {
        unimplemented!()
    }

    pub fn check_expr(&mut self, env: &mut Env, expr: ExprIdx) -> Result<TypeIdx, Error> {
        unimplemented!()
    }
}
