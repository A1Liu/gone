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

    // struct { /* this kind of scope */ }
    // continue/break affect enclosing loop
    StructBody,
}

pub struct Scope {
    pub decls: HashMap<u32, TypeIdx>,
    pub ty: ScopeType,
    pub ty_roots: HashMap<TypeIdx, CodeLoc>,
}

impl Scope {
    pub fn new(ty: ScopeType) -> Self {
        Scope {
            decls: HashMap::new(),
            ty,
            ty_roots: HashMap::new(),
        }
    }
}

pub struct Env<'a>(StackLL<'a, Scope>);

impl<'a> Env<'a> {
    pub fn global() -> Self {
        return Self(StackLL::new(Scope::new(ScopeType::Global)));
    }

    pub fn struct_body<'b>(&'b mut self) -> Env<'b> {
        return Env(self.0.child(Scope::new(ScopeType::StructBody)));
    }
}

#[allow(unused_macros)]
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

pub fn check_ast(ast: &mut Ast) -> Result<(), Error> {
    let (mut env, globals) = (Env::global(), ast.globals);
    return check_scope(ast, &mut env, globals);
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
