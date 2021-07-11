use crate::ast::*;
use crate::buckets::*;
use crate::util::*;
use core::cell::RefCell;
use core::slice;
use lazy_static::lazy_static;

// TODO maybe this should be a handwritten recursive descent parser? It's definitely not worth the
// work now, but it might be if this project ever goes into production.

// TODO eventually this might wanna be a concurrent hashmap, IDK what i wanna do there tbh
pub struct Symbols {
    pub table: HashMap<&'static str, u32>,
    pub translate: Vec<&'static str>,
    pub next: u32,
}

pub const UNDERSCORE_SYMBOL: u32 = 0;
pub const IT_SYMBOL: u32 = 1;

impl Symbols {
    pub fn new(alloc: &impl Allocator<'static>) -> RefCell<Symbols> {
        let mut sym = Symbols {
            table: map(),
            translate: Vec::new(),
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
            let id = self.translate.len() as u32;
            self.table.insert(id_str, id);
            self.translate.push(id_str);
            id
        };

        return id;
    }

    pub fn translate(&self, idx: u32) -> &'static str {
        return self.translate[idx as usize];
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, ()> = {
        let mut map = map();

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
        map.insert("import", ());

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
    let (begin, end) = (x.begin, y.end);
    return span(Expr::Bin(op, a.add(x), a.add(y)), begin, end);
}

peg::parser! {
  pub grammar lang_grammar(a: &BucketList<'static>, sym: &RefCell<Symbols>) for str {
    use peg::ParseLiteral;

    rule nbspace() = [' ' | '\t']+
    rule newline() = ("\n" / "\r\n")+
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
        quiet!{ (nbspace() / block_comment())* }
        (quiet!{ line_comment() } / newline() / semi() / &"}") _

    rule expr_split() =
        quiet!{ (nbspace() / block_comment())* }
        (quiet!{ line_comment() } / newline() / semi() / (&"}" / &")" / &"," / &"]")) _

    rule semi() = ";" _
    rule comma() -> () = nbspace()? "," _

    rule spanned<T>(r: rule<T>) -> Spanned<T> = b:position!() res:r() e:position!() {
        span(res, b, e)
    }

    rule prefixed<T, Ignore>(i: rule<Ignore>, r: rule<T>) -> T = i() res:r() { res }
    rule suffixed<T, Ignore>(r: rule<T>, i: rule<Ignore>) -> T = res:r() i() { res }

    rule run<T>(r: rule<T>) -> () = r() { () }

    rule key(k: &'static str) -> () = ##parse_string_literal(k) !ident_trailing()

    rule strict<T>(r: rule<T>) -> T = res:r() semantic_split() _ { res }

    rule number() -> Spanned<u64> = spanned(<n:$(['0'..='9']+) {? n.parse().or(Err("u64")) }>)
    rule simple_string() -> String = "\"" s:$( ("\\\"" / [^ '"'])* ) "\"" {?
        unescape::unescape(s).ok_or("failed to parse string literal") }
    rule ident_trailing() = ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']
    rule ident_inner() -> Spanned<u32> =
        b:position!()
        id:$(['a'..='z' | 'A'..='Z' | '_'] ident_trailing()*) e:position!() {?
        if KEYWORDS.contains_key(id) {
            Err("ident was keyword")
        } else {
            let id = sym.borrow_mut().add_symbol(a, id);
            Ok(span(id, b, e))
        }
    }
    rule ident() -> Spanned<u32> = quiet!{ ident_inner() } /
        expected!("identifier")



    rule string() -> Spanned<&'static str> =
        begin:position!() s:(simple_string() ++ _) end:position!() {
        let mut len = 0;
        for string in &s { len += string.len(); }
        let mut builder = String::with_capacity(len);
        for string in s { builder += &string; }
        span(&*a.add_str(&builder), begin, end)
    }

    rule paren_expr() -> Spanned<Expr> = "(" _ e:simple_expr() _ ")" { e }

    rule simple_expr() -> Spanned<Expr> = precedence! {
        x:(@) nbspace()? ".." nbspace()? y:@ { bin_op_span(a, BinOp::Range, x, y) }
        x:(@) nbspace()? "..=" nbspace()? y:@ { bin_op_span(a, BinOp::InclusiveRange, x, y) }
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
        b:position!() key("not") _ x:@ { let end = x.end; span(Expr::Not(a.add(x)), b, end) }
        b:position!() "&" nbspace()? x:@ {
            let end = x.end;
            span(Expr::Ref(a.add(x)), b, end) }
        b:position!() "^" nbspace()? x:@ {
            let end = x.end;
            span(Expr::Deref(a.add(x)), b, end) }
        b:position!() ".." nbspace()? x:@ {
            let end = x.end;
            bin_op_span(a, BinOp::Range, span(Expr::NoneValue, b, end), x) }
        b:position!() "..=" _ x:@ {
            let end = x.end;
            bin_op_span(a, BinOp::InclusiveRange, span(Expr::NoneValue, b, end), x) }
        --
        x:@ _ ("." !".") _ field:ident() {
            let (begin, end) = (x.begin, field.end);
            span(Expr::Field{ base: a.add(x), field }, begin, end) }
        x:@ quiet!{ nbspace()? } "(" _ params:(expr() ** comma()) _ ")" e:position!() {
            let begin = x.begin;
            span(Expr::Call(a.add(x), a.add_array(params)), begin, e)
        }
        x:@ nbspace()? ".." &expr_split() e:position!() {
            let begin = x.begin;
            bin_op_span(a, BinOp::Range, x, span(Expr::NoneValue, begin, e)) }
        --
        p:paren_expr() { p }
        s:string() { span(Expr::Str(s.inner), s.begin, s.end) }
        n:number() { span(Expr::Int(n.inner), n.begin, n.end) }
        id:ident() { span(Expr::Ident(id.inner), id.begin, id.end) }
        s:spanned(<key("true") { Expr::True } / key("false") { Expr::False }>) { s }
        s:spanned(<key("None") { Expr::NoneValue }>) { s }
        s:spanned(<".." &expr_split() { Expr::FullRange }>) { s }
    }

    rule complex_expr() -> Spanned<Expr> =
        b:position!() key("if") _ cond:expr() _ if_true:expr() _ key("else") _
        if_false:expr() e:position!() {
            let branch = BranchExpr { cond, if_true, if_false };
            span(Expr::Branch(a.add(branch)), b, e)
        } /
        b:block() { let b = b; span(Expr::Block(b.inner), b.begin, b.end) } /
        m:match_block() { span(Expr::Match(a.add(m.inner)), m.begin, m.end) }

    rule expr() -> Spanned<Expr> =
        e:complex_expr() { e } /
        e:simple_expr() { e }

    rule block() -> Spanned<Block> =
        b:position!() "{" _ stmts:(stmt()*) _ expr:expr()? _ "}" e:position!() {
            let mut stmts = stmts;
            if let Some(expr) = expr {
                stmts.push(span(Stmt::Expr(expr.inner), expr.begin, expr.end));
            }

            span(Block::new(a, stmts), b, e)
        }

    rule match_arm() -> MatchArm = pat:pattern() _ "=>" _ expr:expr() _ comma()? {
        MatchArm { pat, expr }
    }

    rule match_block() -> Spanned<MatchBlock> =
        spanned(<key("match") _ expr:expr() _ "{" _ arms:(match_arm() ** _) _ "}"
            { MatchBlock { expr, arms: a.add_array(arms) } }>)

    rule type_name() -> AstTypeName =
        key("u64") { AstTypeName::U64 } /
        key("string") { AstTypeName::String } /
        key("bool") { AstTypeName::Bool } /
        key("Void") { AstTypeName::NoneType } /
        id:ident() { AstTypeName::Ident(id) } /
        "[" _ ty:type_decl_ref() _ "]" { AstTypeName::Slice(a.add(ty)) } /
        b:block() { AstTypeName::Struct(b.inner) }

    rule type_decl_ref() -> Spanned<AstType> =
        spanned(<ptr:("&"?) _ x:type_name() { AstType { name: x, ptr: ptr.is_some() } }>)

    rule type_decl_enum() -> Vec<Spanned<AstType>> = tys:(type_decl_ref() ++ (_ "|" _)) { tys }

    rule type_decl() -> Spanned<AstType> = tys:type_decl_enum() {?
        let mut tys = tys;
        if tys.len() == 1 {
            Ok(tys.swap_remove(0))
        } else {
            let (begin, end) = (tys[0].begin, tys[tys.len() - 1].end);
            fn has_complex_type(tys: &[Spanned<AstType>]) -> bool {
                for ty in tys {
                    match &ty.inner.name {
                        AstTypeName::Struct(_) => return true,
                        AstTypeName::Slice(ty) => return has_complex_type(slice::from_ref(ty)),
                        _ => {}
                    }
                }

                return false;
            }

            if has_complex_type(&tys) {
                Err("if you don't give your nested types a name, you won't be
                    able to refer to them")
            } else {
                let name = AstTypeName::Enum(a.add_array(tys));
                Ok(span(AstType { name, ptr: false }, begin, end))
            }
        }
    }

    rule pattern_decl() -> PatternStructDecl =
        id:ident() pat:prefixed(<_ ":" _>, <pattern()>)? {
            if let Some(pat) = pat {
                PatternStructDecl::Pattern { id, pat }
            } else {
                PatternStructDecl::Name(id)
            }
    }

    // TODO should we allow complex expressions here?
    rule pattern_name() -> PatternName =
        expr:simple_expr() { PatternName::Expr(expr) } /
        "_" { PatternName::IgnoreValue } /
        "[" _ pats:(pattern() ** comma()) _ "]" { PatternName::Slice(a.add_array(pats)) } /
        "{" _ pats:strict(<pattern_decl()>)*  _ "}" { PatternName::Struct(a.add_array(pats)) }

    rule pattern_ref() -> Spanned<Pattern> =
        b:position!() ptr:("&"?) _ name:pattern_name() e:position!() {
            let pat = Pattern { name, ptr: ptr.is_some() };
            span(pat, b, e)
        }

    rule pattern_enum() -> Vec<Spanned<Pattern>> = pats:(pattern_ref() ++ (_ "|" _)) { pats }

    rule pattern() -> Spanned<Pattern> = pats:pattern_enum() {
        let mut pats = pats;
        if pats.len() == 1 {
            pats.swap_remove(0)
        } else {
            let (begin, end) = (pats[0].begin, pats[pats.len() - 1].end);
            let name = PatternName::Enum(a.add_array(pats));
            span(Pattern { name, ptr: false }, begin, end)
        }
    }

    rule decl_eq_exprs() -> Vec<Spanned<Expr>> =
        "=" _ exprs:(e:complex_expr() { vec![e] } /
            exprs:(simple_expr() ++ comma()) { exprs }) { exprs }

    rule decl_end() -> (Option<Vec<Spanned<AstType>>>, Option<Vec<Spanned<Expr>>>) =
        exprs:decl_eq_exprs() { (None, Some(exprs)) } /
        tys:(type_decl() ++ comma()) _ exprs:decl_eq_exprs()? { (Some(tys), exprs) }

    rule decl() -> Vec<Decl> = ids:(ident() ++ comma()) _ ":" _ end:decl_end() {?
        let (tys, exprs) = end;
        match (tys, exprs) {
            (Some(tys), Some(exprs)) => {
                if ids.len() == tys.len() && tys.len() == exprs.len() {
                    let iter = ids.into_iter().zip(tys.into_iter()).zip(exprs.into_iter());
                    Ok(iter.map(|((id, ty), expr)| Decl { id, ty, expr }).collect())
                } else {
                    Err("declaration length mismatch")
                }
            }
            (None, Some(exprs)) => {
                if ids.len() != exprs.len() {
                    Err("declaration length mismatch")
                } else {
                    let iter = ids.into_iter().zip(exprs.into_iter());
                    let decls = iter.map(|(id, expr)| {
                        let ty = AstType { name: AstTypeName::InferType, ptr: false };
                        let ty = span(ty, id.begin, id.end);
                        return Decl { id, ty, expr };
                    }).collect();

                    Ok(decls)
                }
            }
            (Some(tys), None) => {
                if ids.len() != tys.len() {
                    Err("declaration length mismatch")
                } else {
                    let iter = ids.into_iter().zip(tys.into_iter());
                    let decls = iter.map(|(id, ty)| {
                        let expr = span(Expr::Default, id.begin, id.end);
                        return Decl { id, ty, expr };
                    }).collect();

                    Ok(decls)
                }
            }
            (None, None) => panic!("This shouldn't parse"),
        }
    }

    rule stmt() -> Spanned<Stmt> =
        spanned(<semi() { Stmt::Nop }>) /
        spanned(<key("import") _ path:(ident() ++ (_ "." _)) all:(_ "." _ "*")? {
            Stmt::Import { path: a.add_array(path), all: all.is_some() } }>) /
        spanned(<strict(<d:decl() { Stmt::Decl(a.add_array(d)) }>)>) /
        b:position!() key("type") _ id:ident() _ def:type_decl() {
            let ty = Stmt::Type{ id, def: def.inner };
            span(ty, b, def.end) } /
        b:position!() key("let") _ pat:pattern() _ "=" _ expr:expr() {
            let end = expr.end;
            span(Stmt::Destructure(a.add(Destructure { pat, expr })), b, end) } /
        e:expr() semantic_split() _ { span(Stmt::Expr(e.inner), e.begin, e.end) }

    pub rule stmt_list() -> Block = _ stmts:(stmt() ** _) _ { Block::new(a, stmts) }
  }

}
