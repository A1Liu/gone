use crate::ast::*;
use crate::buckets::*;
use core::cell::RefCell;
use core::slice;
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
        map.insert("bool", ());
        map.insert("Void", ());

        map.insert("true", ());
        map.insert("false", ());
        map.insert("None", ());

        map.insert("let", ());
        map.insert("type", ());

        map.insert("for", ());
        map.insert("while", ());
        map.insert("if", ());
        map.insert("else", ());
        map.insert("match", ());

        map.insert("not", ());
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
    use peg::ParseLiteral;

    rule nbspace() = [' ' | '\t']
    rule newline() = "\n" / "\r\n"
    rule whitespace() = (nbspace() / newline())
    rule line_comment() = "//" [^ '\r' | '\n']* newline()
    rule block_comment_internal() = // TODO do these actually work?
        [^ '*' | '/' ]* "**" block_comment_internal() /
        [^ '*' | '/' ]* "//" block_comment_internal() /
        [^ '*' | '/' ]* block_comment() block_comment_internal() /
        [^ '*' | '/' ]*

    rule block_comment() = "/*" block_comment_internal() "*/"

    rule closing() -> () = &"}" / &")" / &"]"
    rule _() = quiet!{ (whitespace() / line_comment() / block_comment())* }
    rule semantic_split() =
        quiet!{ (nbspace() / block_comment())* }
        (quiet!{ line_comment() } / newline() / semi() / _ closing()) _

    rule semi() = ";" _
    rule comma() -> () = _ "," _

    rule spanned<T>(r: rule<T>) -> Spanned<T> = b:position!() res:r() e:position!() {
        span(res, b, e)
    }

    rule run<T>(r: rule<T>) -> () = r() { () }

    rule key(k: &'static str) -> () = ##parse_string_literal(k) !ident_trailing()

    rule strict<T>(r: rule<T>) -> T = res:r() semantic_split() _ { res }

    rule number() -> Spanned<u64> = spanned(<n:$(['0'..='9']+) {? n.parse().or(Err("u64")) }>)
    rule simple_string() -> String = "\"" s:$( ("\\\"" / [^ '"'])* ) "\"" {?
        unescape::unescape(s).ok_or("failed to parse string literal") }
    rule ident_trailing() = ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']
    rule ident() -> Spanned<u32> =
        b:position!()
        id:$(['a'..='z' | 'A'..='Z' | '_'] ident_trailing()*) e:position!() {?
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

    rule boolean() -> Spanned<bool> = spanned(<key("true") { true } / key("false") { false }>)

    rule none_value() -> Spanned<()> = spanned(<key("None")>)

    rule simple_expr() -> Spanned<Expr> = precedence! {
        x:(@) _ "..<" _ y:@ { bin_op_span(a, BinOp::Range, x, y) }
        x:(@) _ "..=" _ y:@ { bin_op_span(a, BinOp::InclusiveRange, x, y) }
        --
        x:(@) _ key("or") _ y:@ { bin_op_span(a, BinOp::Or, x, y) }
        --
        x:(@) _ key("xor") _ y:@ { bin_op_span(a, BinOp::Xor, x, y) }
        --
        x:(@) _ key("and") _ y:@ { bin_op_span(a, BinOp::And, x, y) }
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
        b:position!() key("not") _ x:@ { span(Expr::Not(a.add(x)), b, x.end) }
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
        b:boolean() { span(if b.inner { Expr::True } else { Expr::False }, b.begin, b.end) }
    }

    rule expr() -> Spanned<Expr> =
        b:position!() key("if") _ cond:expr() _ if_true:expr() _ key("else") _
        if_false:expr() e:position!() {
            let branch = BranchExpr { cond, if_true, if_false };
            span(Expr::Branch(a.add(branch)), b, e)
        } /
        b:block() { span(Expr::Block(b.inner), b.begin, b.end) } /
        m:match_block() { span(Expr::Match(a.add(m.inner)), m.begin, m.end) } /
        e:simple_expr() { e }

    rule block() -> Spanned<&'static [Spanned<Stmt>]> =
        b:position!() "{" _ stmts:(stmt()*) _ expr:expr()? _ "}" e:position!() {
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
        spanned(<key("match") _ expr:expr() _ "{" _ arms:(match_arm() ** _) _ "}"
            { MatchBlock { expr, arms: a.add_array(arms) } }>)

    rule type_name() -> TypeName =
        key("u64") { TypeName::U64 } /
        key("string") { TypeName::String } /
        key("bool") { TypeName::Bool } /
        key("Void") { TypeName::Tuple(&[]) } /
        id:ident() { TypeName::Ident(id) } /
        "$" id:ident() { TypeName::PolymorphDecl(id) } /
        "[" _ ty:type_decl_ref() _ "]" { TypeName::Slice(a.add(ty)) } /
        "{" _ decls:strict(<decl()>)* _ run(<{ dbg!(1) }>) "}" run(<{ dbg!(3) }>) {
            let mut len = 0;
            // let mut last = last.unwrap_or(Vec::new());
            for decl_list in &decls { len += decl_list.len(); }
            // len += last.len();
            let mut decl_out = Vec::with_capacity(len);
            for mut decl_list in decls { decl_out.append(&mut decl_list); }
            // decl_out.append(&mut last);

            TypeName::Struct(&*a.add_array(decl_out))
        } /
        "(" _ tys:(type_decl_ref() ** comma()) _ ")" { TypeName::Tuple(&*a.add_array(tys)) }

    rule type_decl_ref() -> Spanned<Type> =
        spanned(<ptr:("&"?) _ x:type_name() { Type { name: x, ptr: ptr.is_some() } }>)

    rule type_decl_enum() -> Vec<Spanned<Type>> = tys:(type_decl_ref() ++ (_ "|" _)) { tys }

    rule type_decl() -> Spanned<Type> = tys:type_decl_enum() {?
        if tys.len() == 1 {
            Ok(tys[0])
        } else {
            let (begin, end) = (tys[0].begin, tys[tys.len() - 1].end);
            assert!(begin < end);
            fn has_complex_type(tys: &[Spanned<Type>]) -> bool {
                for ty in tys {
                    match ty.inner.name {
                        TypeName::Struct(_)
                        | TypeName::Tuple(_) => return true,
                        TypeName::Slice(ty) => return has_complex_type(slice::from_ref(ty)),
                        _ => {}
                    }
                }

                return false;
            }

            if has_complex_type(&tys) {
                Err("if you don't give your nested types a name, you won't be
                    able to refer to them")
            } else {
                let name = TypeName::Enum(a.add_array(tys));
                Ok(span(Type { name, ptr: false }, begin, end))
            }
        }
    }

    rule pattern_decl() -> PatternStructDecl =
        id:ident() _ ":" _ pat:pattern() { PatternStructDecl { id, pat } }

    rule pattern_name() -> PatternName =
        expr:expr() { PatternName::Expr(expr) } /
        "_" { PatternName::None } /
        "[" _ pats:(pattern() ** comma()) _ "]" { PatternName::Slice(a.add_array(pats)) } /
        "{" _ pats:strict(<pattern_decl()>)*  _ "}" { PatternName::Struct(&*a.add_array(pats)) } /
        "(" _ pats:(pattern() ** comma()) _ ")" { PatternName::Tuple(&*a.add_array(pats)) }

    rule pattern_ref() -> Spanned<Pattern> =
        b:position!() ptr:("&"?) _ name:pattern_name() e:position!() {
            let pat = Pattern { name, ptr: ptr.is_some() };
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
            span(Pattern { name, ptr: false }, begin, end)
        }
    }

    rule decl_eq_exprs() -> Vec<Spanned<Expr>> =
        "=" _ exprs:(expr() ++ comma()) { exprs }

    rule decl_end() -> (Option<Vec<Spanned<Type>>>, Option<Vec<Spanned<Expr>>>) =
        exprs:decl_eq_exprs() { (None, Some(exprs)) } /
        tys:(type_decl() ++ comma()) _ exprs:decl_eq_exprs()? { (Some(tys), exprs) }

    rule decl() -> Vec<Decl> = ids:(ident() ++ comma()) _ ":" _ end:decl_end() {?
        dbg!();
        let (tys, exprs) = end;
        match (tys, exprs) {
            (Some(tys), Some(exprs)) => {
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
            }
            (None, Some(exprs)) => {
                if ids.len() != exprs.len() {
                    Err("declaration length mismatch")
                } else {
                    let mut decls = Vec::with_capacity(ids.len());
                    let iter = ids.into_iter().zip(exprs.into_iter());
                    let ty = Type { name: TypeName::None, ptr: false };
                    let create_ty = |id: Spanned<u32>| span(ty, id.begin, id.end);
                    for (id, expr) in iter {
                        decls.push(Decl { id, ty: create_ty(id), expr });
                    }

                    Ok(decls)
                }
            }
            (Some(tys), None) => {
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
            (None, None) => panic!("This shouldn't parse"),
        }
    }

    rule type_params() -> Vec<Spanned<u32>> =
        "<" _ generics:(ident() ++ comma()) _ ">" { generics }


    rule stmt() -> Spanned<Stmt> =
        spanned(<semi() { Stmt::Nop }>) /
        spanned(<strict(<d:decl() { Stmt::Decl(a.add_array(d)) }>)>) /
        b:position!() key("type") _ id:ident() _ params:type_params()? _ ty:type_decl() {
            let params = params.map(|p| &*a.add_array(p)).unwrap_or(&[]);
            span(Stmt::Type(ty.inner, params), b, ty.end) } /
        b:position!() key("let") _ pat:pattern() _ "=" _ expr:expr() {
            span(Stmt::Destructure(a.add(Destructure { pat, expr })), b, expr.end) } /
        e:expr() semantic_split() _ { span(Stmt::Expr(e.inner), e.begin, e.end) }

    pub rule stmt_list() -> Vec<Spanned<Stmt>> = _ stmts:(stmt() ** _) _ { stmts }
  }

}
