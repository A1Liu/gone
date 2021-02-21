use crate::ast::*;
use crate::util::*;

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

// just does scope checking, without any of the fancy language features associated with scopes
pub fn check_scope(ast: &mut Ast, env: &mut Env, stmts: Range<StmtIdx>) -> Result<(), Error> {
    for stmt in stmts {
        check_stmt(ast, env, stmt)?;
    }

    return Ok(());
}

pub fn check_stmt(ast: &mut Ast, env: &mut Env, stmt: StmtIdx) -> Result<(), Error> {
    match ast[stmt].kind {
        StmtKind::Noop => {}
        StmtKind::Expr(expr) => core::mem::drop(check_expr(ast, env, expr)?),
        StmtKind::Decl(decl) => check_decl(ast, env, decl)?,
        _ => unimplemented!(),
    }

    return Ok(());
}

pub fn check_decl(ast: &mut Ast, env: &mut Env, decl: DeclIdx) -> Result<(), Error> {
    unimplemented!()
}

pub fn check_expr(ast: &mut Ast, env: &mut Env, expr: ExprIdx) -> Result<TypeIdx, Error> {
    unimplemented!()
}
