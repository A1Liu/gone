use crate::ast::*;
use crate::util::*;

// DISJOINT SET UNION FIND ALGORITHM LETS GOOOOOOOO
// (for type inference in one pass)
// Idea: Most types are declared; the few types that arent are almost always inferred
// by just copying the id of the type. However, we have one case that's actually
// complicated, which is typechecking variables that have been assigned integer literals.
// To simplify this case, we require that only integer literals of the same type
// can be added together. This allows us to us a DISJOINT SET UNION FIND to fill in types
//
// Polymorphic functions are handled differently from regular ones
// 1. Copy out type
// 2. Infer types using simple type inference
//
// Polymorphic structs are also handled differently
// 1. Copy out type, with params

pub enum ScopeType {
    // (a: string) => { /* this kind of scope */ }
    // continue and break are disabled
    Function {
        ret_ty: TypeIdx,
        ty_roots: HashMap<TypeIdx, CodeLoc>,
    },

    // The global scope.
    // continue, break, return are disabled
    Global {
        ty_roots: HashMap<TypeIdx, CodeLoc>,
    },

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
}

impl Scope {
    pub fn new(ty: ScopeType) -> Self {
        Scope {
            decls: HashMap::new(),
            ty,
        }
    }
}

pub struct Env<'a>(StackLL<'a, Scope>);

impl<'a> Env<'a> {
    pub fn global() -> Self {
        return Self(StackLL::new(Scope::new(ScopeType::Global {
            ty_roots: HashMap::new(),
        })));
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

pub fn get_type_defn(ast: &mut Ast, env: &mut Env, idx: TypeIdx) -> Result<TypeIdx, Error> {
    while ast[idx].modifiers.len() == 0 {
        // match ast[idx].base {}
    }

    return Ok(idx);
}

pub fn unify_types(
    ast: &mut Ast,
    env: &mut Env,
    src: (TypeIdx, TypeIdx),
    target: TypeIdx,
) -> Result<TypeIdx, Error> {
    return Ok(target);
}

pub fn convert_infer_type(
    ast: &mut Ast,
    env: &mut Env,
    src: TypeIdx,
    target: TypeIdx,
) -> Result<TypeIdx, Error> {
    return Ok(target);
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
