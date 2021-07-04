use crate::ast::*;
use crate::buckets::*;
use core::cell::RefCell;
use lazy_static::lazy_static;
use std::collections::HashMap;

// TODO maybe this should be a handwritten recursive descent parser? It's definitely not worth the
// work now, but it might be if this project ever goes into production.

// TODO eventually this might wanna be a concurrent hashmap, IDK what i wanna do there tbh
pub struct Symbols {
    pub table: HashMap<&'static str, u32>,
    pub next: u32,
}

const UNDERSCORE_SYMBOL: u32 = 0;
const IT_SYMBOL: u32 = 1;

impl Symbols {
    pub fn new(alloc: &impl Allocator<'static>) -> RefCell<Symbols> {
        let mut sym = Symbols {
            table: HashMap::new(),
            next: 0,
        };

        assert_eq!(UNDERSCORE_SYMBOL, sym.add_symbol(alloc, "_"));
        assert_eq!(IT_SYMBOL, sym.add_symbol(alloc, "it"));

        return RefCell::new(sym);
    }

    pub fn add_symbol(&mut self, alloc: &impl Allocator<'static>, id: &str) -> u32 {
        let id = if let Some(&id) = self.table.get(id) {
            id
        } else {
            let id_str = alloc.add_str(id);
            let id = self.next;
            self.table.insert(id_str, id);
            self.next += 1;
            id
        };

        return id;
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, ()> = {
        let mut map = HashMap::new();

        map.insert("u64", ());
        map.insert("string", ());

        map.insert("let", ());
        map.insert("type", ());

        map.insert("for", ());
        map.insert("while", ());
        map.insert("if", ());
        map.insert("else", ());
        map.insert("match", ());

        map.insert("or", ());
        map.insert("xor", ());
        map.insert("and", ());

        map
    };
}

fn bin_op_span(
    a: &impl Allocator<'static>,
    op: BinOp,
    x: Spanned<Expr>,
    y: Spanned<Expr>,
) -> Spanned<Expr> {
    return span(Expr::Bin(op, a.add(x), a.add(y)), x.begin, y.end);
}

peg::parser! {
  pub grammar lang_grammar(a: &BucketList<'static>, sym: &RefCell<Symbols>) for str {
    rule nbspace() = [' ' | '\t']
    rule newline() = "\n" / "\r\n"
    rule whitespace() = (nbspace() / newline())+
    rule line_comment() = "//" [^ '\r' | '\n']* newline()
    rule block_comment_internal() = // TODO do these actually work?
        [^ '*' | '/' ]* "**" block_comment_internal() /
        [^ '*' | '/' ]* "//" block_comment_internal() /
        [^ '*' | '/' ]* block_comment() block_comment_internal() /
        [^ '*' | '/' ]*

    rule block_comment() = "/*" block_comment_internal() "*/"

    rule _() = quiet!{ (whitespace() / line_comment() / block_comment())* }
    rule semantic_split() =
        quiet!{ (nbspace() / block_comment())* } (line_comment() / newline() / semi())

    rule semi() = ";" _
    rule comma() -> () = _ "," _

    rule number() -> Spanned<u64> = b:position!() n:$(['0'..='9']+) e:position!()
    {? n.parse().map(|n| span(n, b, e)).or(Err("u64")) }
    rule simple_string() -> String = "\"" s:$( ("\\\"" / [^ '"'])* ) "\"" {?
        unescape::unescape(s).ok_or("failed to parse string literal") }
    rule ident_trailing() = ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']
    rule ident() -> Spanned<u32> =
        b:position!()
        id:$(['a'..='z' | 'A'..='Z' | '_'] ident_trailing()*)
        e:position!() {?
        if KEYWORDS.contains_key(id) {
            Err("ident was keyword")
        } else {
            Ok(span(sym.borrow_mut().add_symbol(a, id), b, e))
        }
    }

    rule string() -> Spanned<&'static str> =
        begin:position!() s:(simple_string() ++ _) end:position!() {
        let mut len = 0;
        for string in &s { len += string.len(); }
        let mut builder = String::with_capacity(len);
        for string in s { builder += &string; }
        span(&*a.add_str(&builder), begin, end)
    }

    rule paren_expr() -> Spanned<Expr> =
        b:position!() "(" _ es:(simple_expr() ** comma()) force:comma()? _ ")" e:position!() {
            match es.len() {
                1 if !force.is_some() => es[0],
                _ => span(Expr::Tuple(a.add_array(es)), b, e),
            }
        }

    rule simple_expr() -> Spanned<Expr> = precedence! {
        x:(@) _ "..<" _ y:@ { bin_op_span(a, BinOp::Range, x, y) }
        x:(@) _ "..=" _ y:@ { bin_op_span(a, BinOp::InclusiveRange, x, y) }
        --
        x:(@) _ "or" _ y:@ { bin_op_span(a, BinOp::Or, x, y) }
        --
        x:(@) _ "xor" _ y:@ { bin_op_span(a, BinOp::Xor, x, y) }
        --
        x:(@) _ "and" _ y:@ { bin_op_span(a, BinOp::And, x, y) }
        --
        x:(@) _ "!=" _ y:@ { bin_op_span(a, BinOp::Neq, x, y) }
        x:(@) _ "==" _ y:@ { bin_op_span(a, BinOp::Eq, x, y) }
        --
        x:(@) _ ">"  _ y:@ { bin_op_span(a, BinOp::Gt, x, y) }
        x:(@) _ "<"  _ y:@ { bin_op_span(a, BinOp::Lt, x, y) }
        x:(@) _ ">=" _ y:@ { bin_op_span(a, BinOp::Geq, x, y) }
        x:(@) _ "<=" _ y:@ { bin_op_span(a, BinOp::Leq, x, y) }
        --
        x:(@) _ ">>" _ y:@ { bin_op_span(a, BinOp::RShift, x, y) }
        x:(@) _ "<<" _ y:@ { bin_op_span(a, BinOp::LShift, x, y) }
        --
        x:(@) _ "+" _ y:@ { bin_op_span(a, BinOp::Add, x, y) }
        --
        x:(@) _ "*" _ y:@ { bin_op_span(a, BinOp::Mult, x, y) }
        --
        b:position!() "&" _ x:@ { span(Expr::Ref(a.add(x)), b, x.end) }
        b:position!() "^" _ x:@ { span(Expr::Deref(a.add(x)), b, x.end) }
        --
        x:@ _ "." _ id:ident() { span(Expr::Field(a.add(x), id), x.begin, id.end) }
        x:@ quiet!{ nbspace()* } "(" _ params:(expr() ** comma()) _ ")" e:position!() {
            span(Expr::Call(a.add(x), a.add_array(params)), x.begin, e)
        }
        x:@ _ "..." e:position!() { span(Expr::Splat(a.add(x)), x.begin, e) }
        --
        p:paren_expr() { p }
        s:string() { span(Expr::Str(s.inner), s.begin, s.end) }
        n:number() { span(Expr::Int(n.inner), n.begin, n.end) }
        id:ident() { span(Expr::Ident(id.inner), id.begin, id.end) }
    }

    rule expr() -> Spanned<Expr> =
        b:position!() "if" _ cond:expr() _ if_true:expr() _ "else" _
        if_false:expr() e:position!() {
            let branch = BranchExpr { cond, if_true, if_false };
            span(Expr::Branch(a.add(branch)), b, e)
        } /
        b:block() { span(Expr::Block(b.inner), b.begin, b.end) } /
        m:match_block() { span(Expr::Match(a.add(m.inner)), m.begin, m.end) } /
        e:simple_expr() { e }

    rule block() -> Spanned<&'static [Spanned<Stmt>]> =
        b:position!() "{" _ stmts:(stmt()*) expr:expr()? _ "}" e:position!() {
            let mut stmts = stmts;
            if let Some(expr) = expr {
                stmts.push(span(Stmt::Expr(expr.inner), expr.begin, expr.end));
            }
            span(&*a.add_array(stmts), b, e)
        }

    rule match_arm() -> MatchArm = pat:pattern() _ "=>" _ expr:expr() _ comma()? {
        MatchArm { pat, expr }
    }

    rule match_block() -> Spanned<MatchBlock> =
        b:position!() "match" _ expr:expr() _ "{" _
        arms:(match_arm() ** _) _ "}" e:position!() {
        span(MatchBlock { expr, arms: a.add_array(arms) }, b, e)
    }


    rule type_name() -> TypeName =
        "u64" !ident_trailing() { TypeName::U64 } /
        "string" !ident_trailing() { TypeName::String } /
        id:ident() { TypeName::Ident(id) } /
        "$" id:ident() { TypeName::PolymorphDecl(id) } /
        "[" _ ty:type_decl() _ "]" { TypeName::Slice(a.add(ty)) } /
        "{" _ decls:decl()*  _ "}" {
            let mut len = 0;
            for decl_list in &decls { len += decl_list.len(); }
            let mut decl_out = Vec::with_capacity(len);
            for mut decl_list in decls { decl_out.append(&mut decl_list); }

            TypeName::Struct(&*a.add_array(decl_out))
        } /
        "(" _ tys:(type_decl() ** comma()) _ ")" { TypeName::Tuple(&*a.add_array(tys)) }

    rule type_decl_ref() -> Spanned<Type> =
        b:position!() ptr:("&"?) _ x:type_name() e:position!() {
            let ty = Type { name: x, pointer: ptr.is_some() };
            span(ty, b, e)
        }

    rule type_decl_enum() -> Vec<Spanned<Type>> =
        x:type_decl_ref() _ "|" _  rest:type_decl_enum() {
            let mut rest = rest; // NOTE This should put things in reverse order
            rest.push(x);
            rest
        } /
        x:type_decl_ref() { vec![x] }

    rule type_decl() -> Spanned<Type> = tys:type_decl_enum() {?
        if tys.len() == 1 {
            Ok(tys[0])
        } else {
            let (begin, end) = (tys[tys.len() - 1].begin, tys[0].end);
            assert!(begin < end);
            let has_complex_type = |tys: &[Spanned<Type>]| -> bool {
                for ty in tys {
                    match ty.inner.name {
                        TypeName::Slice(_)
                        | TypeName::Struct(_)
                        | TypeName::Tuple(_) => return true,
                        _ => {}
                    }
                }

                return false;
            };

            if has_complex_type(&tys) {
                Err("dude make an alias")
            } else {
                let name = TypeName::Enum(a.add_array(tys));
                Ok(span(Type { name, pointer: false }, begin, end))
            }
        }
    }

    rule pattern_decl() -> PatternStructDecl =
        id:ident() _ ":" _ pat:pattern() _ semi()? { PatternStructDecl { id, pat } }

    rule pattern_name() -> PatternName =
        expr:expr() { PatternName::Expr(expr) } /
        "_" { PatternName::None } /
        "[" _ pats:(pattern() ** comma()) _ "]" { PatternName::Slice(a.add_array(pats)) } /
        "{" _ pats:pattern_decl()*  _ "}" { PatternName::Struct(&*a.add_array(pats)) } /
        "(" _ pats:(pattern() ** comma()) _ ")" { PatternName::Tuple(&*a.add_array(pats)) }

    rule pattern_ref() -> Spanned<Pattern> =
        b:position!() ptr:("&"?) _ name:pattern_name() e:position!() {
            let pat = Pattern { name, pointer: ptr.is_some() };
            span(pat, b, e)
        }

    rule pattern_enum() -> Vec<Spanned<Pattern>> =
        x:pattern_ref() _ "|" _  rest:pattern_enum() {
            let mut rest = rest; // NOTE This should put things in reverse order
            rest.push(x);
            rest
        } /
        x:pattern_ref() { vec![x] }

    rule pattern() -> Spanned<Pattern> = pats:pattern_enum() {
        if pats.len() == 1 {
            pats[0]
        } else {
            let (begin, end) = (pats[pats.len() - 1].begin, pats[0].end);
            assert!(begin < end);
            let name = PatternName::Enum(a.add_array(pats));
            span(Pattern { name, pointer: false }, begin, end)
        }
    }

    rule decl_end() -> Vec<Spanned<Expr>> = "=" _ exprs:(expr() ++ comma()) { exprs }

    rule decl() -> Vec<Decl> = ids:(ident() ++ comma()) _ ":" _ exprs:decl_end() _ semi()? {?
        if ids.len() != exprs.len() {
            Err("declaration length mismatch")
        } else {
            let mut decls = Vec::with_capacity(ids.len());
            let iter = ids.into_iter().zip(exprs.into_iter());
            let ty = Type { name: TypeName::None, pointer: false };
            let create_ty = |id: Spanned<u32>| span(ty, id.begin, id.end);
            for (id, expr) in iter {
                decls.push(Decl { id, ty: create_ty(id), expr });
            }

            Ok(decls)
        }
    } /
        ids:(ident() ++ comma()) _ ":" _ tys:(type_decl() ++ comma()) _
        exprs:decl_end() _ semi()? {?
        if ids.len() == tys.len() && tys.len() == exprs.len() {
            let mut decls = Vec::with_capacity(ids.len());
            let iter = ids.into_iter().zip(tys.into_iter()).zip(exprs.into_iter());
            for ((id, ty), expr) in iter {
                decls.push(Decl { id, ty, expr });
            }

            Ok(decls)
        } else {
            Err("declaration length mismatch")
        }
    } /
        ids:(ident() ++ comma()) _ ":" _ tys:(type_decl() ++ comma()) _ semi()? {?
        if ids.len() != tys.len() {
            Err("declaration length mismatch")
        } else {
            let mut decls = Vec::with_capacity(ids.len());
            let iter = ids.into_iter().zip(tys.into_iter());
            let create_expr = |id: Spanned<u32>| span(Expr::Default, id.begin, id.end);
            for (id, ty) in iter {
                decls.push(Decl { id, ty, expr: create_expr(id) });
            }

            Ok(decls)
        }
    }

    rule type_params() -> Vec<Spanned<u32>> =
        "<" _ generics:(ident() ** comma()) _ ">" { generics }


    rule stmt() -> Spanned<Stmt> =
        b:position!() semi() e:position!() { span(Stmt::Nop, b, e) } /
        b:position!() d:decl() e:position!() { span(Stmt::Decl(a.add_array(d)), b, e) } /
        b:position!() "type" _ id:ident() _ params:type_params()? _ ty:type_decl() {
            let params = params.map(|p| &*a.add_array(p)).unwrap_or(&[]);
            span(Stmt::Type(ty.inner, params), b, ty.end) } /
        b:position!() "let" _ pat:pattern() _ "=" _ expr:expr() {
            span(Stmt::Destructure(a.add(Destructure { pat, expr })), b, expr.end) } /
        e:expr() semantic_split() _ { span(Stmt::Expr(e.inner), e.begin, e.end) }

    pub rule stmt_list() -> Vec<Spanned<Stmt>> = _ stmts:(stmt() ** _) _ { stmts }
  }

}
