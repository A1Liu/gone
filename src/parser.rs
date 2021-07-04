use crate::ast::*;
use crate::buckets::*;
use core::cell::RefCell;
use lazy_static::lazy_static;
use std::collections::HashMap;

// TODO eventually this might wanna be a concurrent hashmap, IDK what i wanna do there tbh
pub struct Symbols {
    pub table: HashMap<&'static str, u32>,
    pub next: u32,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, ()> = {
        let mut map = HashMap::new();
        map.insert("u64", ());
        map.insert("string", ());

        map.insert("var", ());
        map.insert("const", ());

        map.insert("_", ());
        map.insert("for", ());
        map.insert("or", ());
        map.insert("xor", ());
        map.insert("and", ());
        map.insert("if", ());
        map.insert("then", ());
        map.insert("else", ());

        map
    };
}

peg::parser! {
  grammar parser(a: &BucketList<'static>, sym: &RefCell<Symbols>) for str {
    rule _() = [' ' | '\t' | '\n' | '\r']*
    rule semi() = _? ";" _?
    rule comma() = _? "," _?

    rule number() -> Spanned<u64> = b:position!() n:$(['0'..='9']+) e:position!()
    {? n.parse().map(|n| span(n, b, e)).or(Err("u64")) }
    rule simple_string() -> &'input str = "\"" s:$( ("\\\"" / [^ '"'])* ) "\"" { s }
    rule ident() -> Spanned<u32> =
        b:position!()
        id:$(['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '_' | '0'..='9'])
        e:position!() {?
        if KEYWORDS.contains_key(id) {
            Err("ident was keyword")
        } else {
            let mut sym = sym.borrow_mut();
            let id = if let Some(&id) = sym.table.get(id) {
                id
            } else {
                let id_str = a.add_str(id);
                let id = sym.next;
                sym.table.insert(id_str, id);
                sym.next += 1;
                id
            };

            Ok(span(id, b, e))
        }
    }

    rule string() -> Spanned<&'static str> =
        begin: position!() s:(simple_string() ++ _) end:position!() {
        let mut len = 0;
        for string in &s { len += string.len(); }
        let mut builder = String::with_capacity(len);
        for string in s { builder.push_str(string); }
        span(&*a.add_str(&builder), begin, end)
    }

    rule simple_expr() -> Spanned<Expr> = precedence! {
        x:(@) _ "or" _ y:@ { span(Expr::Or(a.add(x), a.add(y)), x.begin, y.end) }
        --
        x:(@) _ "xor" _ y:@ { span(Expr::Xor(a.add(x), a.add(y)), x.begin, y.end) }
        --
        x:(@) _ "and" _ y:@ { span(Expr::And(a.add(x), a.add(y)), x.begin, y.end) }
        --
        x:(@) _ "!=" _ y:@ { span(Expr::Neq(a.add(x), a.add(y)), x.begin, y.end) }
        x:(@) _ "==" _ y:@ { span(Expr::Eq(a.add(x), a.add(y)), x.begin, y.end) }
        --
        x:(@) _ ">"  _ y:@ { span(Expr::Gt(a.add(x), a.add(y)), x.begin, y.end) }
        x:(@) _ "<"  _ y:@ { span(Expr::Lt(a.add(x), a.add(y)), x.begin, y.end) }
        x:(@) _ ">=" _ y:@ { span(Expr::Geq(a.add(x), a.add(y)), x.begin, y.end) }
        x:(@) _ "<=" _ y:@ { span(Expr::Leq(a.add(x), a.add(y)), x.begin, y.end) }
        --
        x:(@) _ ">>" _ y:@ { span(Expr::RShift(a.add(x), a.add(y)), x.begin, y.end) }
        x:(@) _ "<<" _ y:@ { span(Expr::LShift(a.add(x), a.add(y)), x.begin, y.end) }
        --
        x:(@) _ "+" _ y:@ { span(Expr::Add(a.add(x), a.add(y)), x.begin, y.end) }
        --
        b:position!() "&" _ x:@ { span(Expr::Ref(a.add(x)), b, x.end) }
        b:position!() "^" _ x:@ { span(Expr::Deref(a.add(x)), b, x.end) }
        --
        x:@ _ "." _ id:ident() { span(Expr::Field(a.add(x), id), x.begin, id.end) }
        x:@ _ "(" _ params:(expr() ** comma()) _ ")" e:position!() {
            span(Expr::Call(a.add(x), a.add_array(params)), x.begin, e)
        }
        --
        "(" _ e:simple_expr() _ ")" { e }
        s:string() { span(Expr::Str(s.inner), s.begin, s.end) }
        n:number() { span(Expr::Int(n.inner), n.begin, n.end) }
        id:ident() { span(Expr::Ident(id.inner), id.begin, id.end) }
    }

    rule expr() -> Spanned<Expr> =
        b:position!() "if" _ cond:expr() _ "then" _ if_true:expr() _
        "else" _ if_false:expr() e:position!() {
            let branch = BranchExpr { cond, if_true, if_false };
            span(Expr::Branch(a.add(branch)), b, e)
        } /
        b:block() { span(Expr::Block(b.inner), b.begin, b.end) } /
        e:simple_expr() { e }

    rule block() -> Spanned<&'static [Spanned<Stmt>]> =
        b:position!() "{" _ stmts:(stmt()*) expr:expr()? _ "}" e:position!() {
            let mut stmts = stmts;
            if let Some(expr) = expr {
                stmts.push(span(Stmt::Expr(expr.inner), expr.begin, expr.end));
            }
            span(&*a.add_array(stmts), b, e)
        }

    rule type_name() -> TypeName =
        "_" { TypeName::None } /
        "u64" { TypeName::U64 } /
        "string" { TypeName::U64 } /
        "[" _ ty:type_decl() _ "]" { TypeName::Slice(a.add(ty)) } /
        id:ident() { TypeName::Ident(id) }

    rule type_decl() -> Spanned<Type> =
        b:position!() ptr:("&"?) name:type_name() e:position!() {
            let ty = Type { name, pointer: ptr.is_some() };
            span(ty, b, e)
        }

    rule stmt() -> Spanned<Stmt> =
        b:position!() ";" e:position!() { span(Stmt::Nop, b, e) } /
        b:position!() ids:(ident() ++ comma()) ":" tys:(type_decl() ** comma()) "="
        exprs:(expr() ++ comma()) semi() e:position!() {?
            if ids.len() != exprs.len() {
                Err("declaration length mismatch")
            } else if tys.len() != ids.len() && tys.len() != 0 {
                Err("declaration length mismatch")
            } else if tys.len() == 0 {
                let mut decls = Vec::with_capacity(ids.len());
                let iter = ids.into_iter().zip(exprs.into_iter());
                let ty = Type { name: TypeName::None, pointer: false };
                let create_ty = |id: Spanned<u32>| span(ty, id.begin, id.end);
                for (id, expr) in iter {
                    decls.push(Decl { id, ty: create_ty(id), expr });
                }

                Ok(span(Stmt::Decl(a.add_array(decls)), b, e))
            } else {
                let mut decls = Vec::with_capacity(ids.len());
                let iter = ids.into_iter().zip(tys.into_iter()).zip(exprs.into_iter());
                for ((id, ty), expr) in iter {
                    decls.push(Decl { id, ty, expr });
                }

                Ok(span(Stmt::Decl(a.add_array(decls)), b, e))
            }
        } /
        b:block() { span(Stmt::Block(b.inner), b.begin, b.end) } /
        e:expr() semi() { span(Stmt::Expr(e.inner), e.begin, e.end) } /
        b:position!() "if" _ cond:expr() _ if_true:stmt() _
        "else" _ if_false:stmt() e:position!() {
            let branch = BranchStmt { cond, if_true, if_false };
            span(Stmt::Branch(a.add(branch)), b, e)
        }
  }
}
