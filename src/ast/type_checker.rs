use crate::ast::*;
use crate::util::*;

// Ideally the programmer should be able to say the following things without the
// language getting in the way:
//
// 1. struct polymorphism - "this set of structs will behave the same, but with
//    certain key differences"
// 2. procedure polymorphism - "this function operates on all structs that behave a certain way"
// 3. type information - "print out the type of this object at compile time"
// 4. runtime type information - "print out the type of this object at runtime"
// 5. serialization - "serialize this struct, and deserialize it later"
// 6. compile time execution - "run this at compile time"
// 7. type inference - "the type of this object is stated when the object is used"
//
// Solution 1: Separate CTFE and Type systems, types are not values for CTFE
// Right now we have a mostly unified syntax for doing runtime type information.
// making these things separate systems probably requires building a new typed
// AST from the original AST to do things like differentiate between struct declarations
// and expressions. Additionally, the semantics of the program will be more limited;
// You won't, for example, be able to declare the type of a variable to be the
// result of a compile-time function call. Maybe that's ok though. On the bright
// side, this kind of system would likely be much easier to implement.
//
// Solution 2: Unified system, types are values for CTFE
// This lends well to the unified syntax, and supports much more, but is much harder
// to implement. We have to do the following steps:
//
// - Determine typecheck/execution order - CTFE functions need to be type checked
//    first, and may call each other, so they'll need to be type checked in waves.
// - Interleave type checking and constant function evaluation
//
// Type check into TCAst, which is the thing thats executed. TCAst is more C-like,
// with all control flow desugared and the implicit structures (tuples and scopes)
// made explicit. It also will do things like link directly to declarations instead
// of using identifier symbols
//
// DISJOINT SET UNION FIND ALGORITHM LETS GOOOOOOOO
// (for type inference in 2 passes)
// Idea: Most types are declared; the few types that arent are almost always inferred
// by just copying the id of the type. To solve the cases where types must actually
// be inferred, we use a DISJOINT SET UNION FIND to indicate a type is equivalent
// to another type, and use the INFER_TYPE constant to indicate a type is not solved
// yet.
//

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
    unimplemented!()
}

pub fn check_decl(ast: &mut Ast, env: &mut Env, decl: DeclIdx) -> Result<(), Error> {
    unimplemented!()
}

pub fn check_expr(ast: &mut Ast, env: &mut Env, expr: ExprIdx) -> Result<TypeIdx, Error> {
    unimplemented!()
}
