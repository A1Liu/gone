use super::tree::*;
use crate::buckets::*;
use crate::filedb::*;
use crate::util::*;
use core::marker::PhantomData;

/*
pub struct ParseEnv {
    pub ast: Ast,
    pub symbols: Symbols,
}

peg::parser! {
// match parser::rule(string, env) {}
pub grammar parser(env: &mut ParseEnv) for str {

rule w() = [' ' | '\t']*

rule number() -> u64 = n:$(['0'..='9']+) {? n.parse().or(Err("number too large")) }

}
}
*/

pub fn parse_file(lexer: &mut Lexer, id: u32, file: &str) -> Result<Ast, Error> {
    let mut parser = Parser::new(id, lexer.lex(file));
    parser.eat_newline();
    // let toks: Vec<_> = parser.tokens.iter().map(|a| a.kind).collect();
    // println!("{:?}", toks);

    let stmts = parser.parse_stmts()?;
    parser.ast.globals = parser.ast.add_stmts(stmts);
    return Ok(parser.ast);
}

pub struct Parser<'data> {
    pub tokens: Vec<Token<'data>>,
    pub current: usize,
    pub ast: Ast,
}

impl<'data> Parser<'data> {
    pub fn new(file: u32, tokens: impl Iterator<Item = Token<'data>>) -> Self {
        Self {
            ast: Ast::new(file),
            tokens: tokens.collect(),
            current: 0,
        }
    }

    pub fn e(&mut self, kind: ExprKind, loc: CodeLoc) -> Expr {
        return self.ast.e(kind, loc);
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, Error> {
        match self.peek_err()?.kind {
            TokenKind::For => {
                let start = self.pop().unwrap().loc;

                let first = self.parse_expr()?;
                self.eat_newline();
                let in_tok = self.peek_err()?;

                if in_tok.kind == TokenKind::In {
                    self.pop().unwrap();

                    let second = self.parse_expr()?;
                    self.eat_newline();
                    let body = self.parse_stmt()?;

                    return Ok(Stmt {
                        loc: l_from(start, body.loc),
                        kind: StmtKind::For {
                            iter: self.ast.add_expr(first),
                            source: self.ast.add_expr(second),
                            body: self.ast.add_stmt(body),
                        },
                    });
                }

                let body = self.parse_stmt()?;

                let iter = self.e(ExprKind::Ident(BuiltinSymbol::It as u32), first.loc);

                return Ok(Stmt {
                    loc: l_from(start, body.loc),
                    kind: StmtKind::For {
                        iter: self.ast.add_expr(iter),
                        source: self.ast.add_expr(first),
                        body: self.ast.add_stmt(body),
                    },
                });
            }

            TokenKind::If => {
                let start = self.pop().unwrap().loc;

                let if_cond = self.parse_expr()?;
                self.eat_newline();

                let if_body = self.parse_stmt()?;
                self.eat_newline();

                let (loc, else_body) = if let Some(Token {
                    kind: TokenKind::Else,
                    ..
                }) = self.peek()
                {
                    self.pop().unwrap();
                    self.eat_newline();
                    let else_body = self.parse_stmt()?;
                    let (loc, else_body) = (else_body.loc, self.ast.add_stmt(else_body));
                    (l_from(start, loc), Some(else_body))
                } else {
                    (l_from(start, if_body.loc), None)
                };

                return Ok(Stmt {
                    loc,
                    kind: StmtKind::Branch {
                        if_cond: self.ast.add_expr(if_cond),
                        if_body: self.ast.add_stmt(if_body),
                        else_body,
                    },
                });
            }

            TokenKind::Semicolon | TokenKind::Newline => {
                return Ok(Stmt {
                    kind: StmtKind::Noop,
                    loc: self.pop().unwrap().loc,
                });
            }

            _ => {
                let expr = self.parse_expr()?;
                self.eat_line_ending();

                let (loc, expr) = (expr.loc, self.ast.add_expr(expr));

                return Ok(Stmt {
                    kind: StmtKind::Expr(expr),
                    loc,
                });
            }
        }
    }

    #[inline]
    pub fn is_decl(&mut self) -> bool {
        let tok = match self.peek() {
            Some(t) => t,
            None => return false,
        };

        if let TokenKind::Ident(_) = tok.kind {
        } else {
            return false;
        }

        let tok = match self.peek_2() {
            Some(tok) => tok,
            None => return false,
        };

        return tok.kind == TokenKind::Colon;
    }

    pub fn expect_decl(&mut self) -> Result<Decl, Error> {
        let tok = self.pop_err()?;
        let err_message = "expected identifier to begin declaration";
        let (ident, loc) = match tok.kind {
            TokenKind::Ident(sym) => (sym, tok.loc),
            _ => return Err(self.err(err_message, tok.loc, here!())),
        };

        let tok = self.pop_err()?;
        if tok.kind != TokenKind::Colon {
            return Err(self.err("expected this to be a ':' token", tok.loc, here!()));
        }

        self.eat_newline();
        let tok = self.peek_err()?;
        let ty = if tok.kind != TokenKind::Eq {
            let (ty, ty_loc) = self.parse_type()?;
            let (loc, ty, expr) = (l_from(loc, ty_loc), self.ast.add_ty(ty), None);
            self.eat_newline();

            if let Some(tok) = self.peek() {
                if tok.kind != TokenKind::Eq {
                    #[rustfmt::skip]
                    return Ok(Decl { ident, ty, expr, loc, });
                }
            }

            ty
        } else {
            self.ast.add_ty(INFER_TYPE)
        };

        let tok = self.expect_tok(TokenKind::Eq, "expected an '=' token")?;
        self.eat_newline();
        let expr = self.parse_expr()?;
        let (loc, expr) = (l_from(loc, expr.loc), Some(self.ast.add_expr(expr)));
        #[rustfmt::skip]
        return Ok(Decl { ident, ty, expr, loc, });
    }

    #[inline]
    pub fn parse_expr(&mut self) -> Result<Expr, Error> {
        return self.parse_assign();
    }

    pub fn parse_assign(&mut self) -> Result<Expr, Error> {
        let left = self.parse_ternary()?;

        let tok = match self.peek() {
            Some(t) => t,
            None => return Ok(left),
        };

        match tok.kind {
            TokenKind::Eq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::Assign(pair), loc));
            }
            TokenKind::PlusEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::Add, pair), loc));
            }
            TokenKind::DashEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::Sub, pair), loc));
            }
            TokenKind::StarEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);

                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::Mul, pair), loc));
            }
            TokenKind::SlashEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);

                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::Div, pair), loc));
            }
            TokenKind::PercentEq => {
                self.pop().unwrap();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);

                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::Mod, pair), loc));
            }
            TokenKind::LtLtEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);

                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::LShift, pair), loc));
            }

            TokenKind::GtGtEq => {
                self.pop().unwrap();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);

                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::RShift, pair), loc));
            }

            TokenKind::AmpEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);

                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::BitAnd, pair), loc));
            }
            TokenKind::CaretEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);

                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::BitXor, pair), loc));
            }
            TokenKind::LineEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);

                let pair = self.ast.add_pair(left, right);
                return Ok(self.e(ExprKind::MutAssign(BinOp::BitOr, pair), loc));
            }
            _ => return Ok(left),
        }
    }

    pub fn parse_ternary(&mut self) -> Result<Expr, Error> {
        let condition = self.parse_bool_or()?;

        let question_tok = match self.peek() {
            Some(tok) => tok,
            None => return Ok(condition),
        };

        if question_tok.kind != TokenKind::Question {
            return Ok(condition);
        }

        self.pop().unwrap();
        self.eat_newline();

        let if_true = self.parse_expr()?;
        self.eat_newline();

        let colon_tok = self.pop_err()?;
        if colon_tok.kind != TokenKind::Colon {
            let message = "expected ':' token, got something else instead";
            return Err(self.err(message, colon_tok.loc, here!()));
        }

        self.eat_newline();
        let if_false = self.parse_bool_or()?;

        let loc = l_from(condition.loc, if_false.loc);

        let ternary = self.ast.add_ternary(condition, if_true, if_false);
        return Ok(self.e(ExprKind::Ternary(ternary), loc));
    }

    pub fn parse_bool_or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_bool_and()?;
        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match tok.kind {
                TokenKind::LineLine => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_bool_and()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::BoolOr, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_bool_and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_bit_or()?;
        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match tok.kind {
                TokenKind::AmpAmp => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_bit_or()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::BoolAnd, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_bit_or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_bit_xor()?;
        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match tok.kind {
                TokenKind::Line => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_bit_xor()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::BitOr, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_bit_xor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_bit_and()?;
        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match tok.kind {
                TokenKind::Caret => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_bit_and()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::BitXor, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_bit_and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_equality()?;
        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match tok.kind {
                TokenKind::Amp => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_equality()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::BitAnd, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_comparison()?;
        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match tok.kind {
                TokenKind::EqEq => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_comparison()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Eq, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Neq => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_comparison()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Neq, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_shift()?;

        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match tok.kind {
                TokenKind::Lt => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Lt, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Leq => {
                    self.pop().unwrap();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Leq, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Gt => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Gt, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Geq => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Geq, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_shift(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_add()?;
        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match self.peek_err()?.kind {
                TokenKind::GtGt => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_add()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::RShift, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::LtLt => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_add()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::LShift, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_add(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_multiply()?;

        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match self.peek_err()?.kind {
                TokenKind::Plus => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_multiply()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Add, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Dash => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_multiply()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Sub, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_multiply(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_prefix()?;

        while let Some(tok) = self.peek() {
            let start_loc = expr.loc;
            match self.peek_err()?.kind {
                TokenKind::Slash => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_prefix()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Div, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Star => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_prefix()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Mul, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Percent => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_prefix()?;
                    let end_loc = right.loc;

                    let kind = ExprKind::BinOp(BinOp::Mod, self.ast.add_pair(expr, right));
                    expr = self.e(kind, l_from(start_loc, end_loc));
                }
                _ => return Ok(expr),
            }
        }

        return Ok(expr);
    }

    pub fn parse_prefix(&mut self) -> Result<Expr, Error> {
        let tok = self.peek_err()?;
        match tok.kind {
            TokenKind::Amp => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(self.e(ExprKind::UnaryOp(UnaryOp::Ref, target), loc));
            }
            TokenKind::Star => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(self.e(ExprKind::UnaryOp(UnaryOp::Deref, target), loc));
            }

            TokenKind::Bang => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(self.e(ExprKind::UnaryOp(UnaryOp::BoolNot, target), loc));
            }
            TokenKind::Tilde => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(self.e(ExprKind::UnaryOp(UnaryOp::BitNot, target), loc));
            }
            TokenKind::Dash => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(self.e(ExprKind::UnaryOp(UnaryOp::Neg, target), loc));
            }

            _ => return self.parse_range(),
        }
    }

    pub fn parse_range(&mut self) -> Result<Expr, Error> {
        let start = self.parse_postfix()?;

        let tok = match self.peek() {
            Some(tok) => tok,
            None => return Ok(start),
        };

        if tok.kind != TokenKind::DotDotDot {
            return Ok(start);
        }

        self.pop().unwrap();

        let end = self.parse_postfix()?;
        let loc = l_from(start.loc, end.loc);
        let (start, end) = (self.ast.add_expr(start), self.ast.add_expr(end));

        let range = self.e(ExprKind::Range(start, end), loc);

        let tok = match self.peek() {
            Some(tok) => tok,
            None => return Ok(range),
        };

        if tok.kind == TokenKind::DotDotDot {
            return Err(self.err("'...' operator cannot be chained", loc, here!()));
        }

        return Ok(range);
    }

    pub fn parse_postfix(&mut self) -> Result<Expr, Error> {
        let mut operand = self.parse_atom()?;
        let start_loc = operand.loc;

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::LParen => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let mut func_and_params = vec![operand];
                    let rparen_tok = self.peek_err()?;

                    if rparen_tok.kind != TokenKind::RParen {
                        let param = self.parse_expr()?;
                        func_and_params.push(param);
                        self.eat_newline();
                        let mut comma_tok = self.peek_err()?;

                        while comma_tok.kind == TokenKind::Comma {
                            self.pop().unwrap();
                            self.eat_newline();
                            func_and_params.push(self.parse_expr()?);
                            self.eat_newline();
                            comma_tok = self.peek_err()?;
                        }

                        if comma_tok.kind != TokenKind::RParen {
                            let message = "unexpected token when parsing end of function call";
                            return Err(self.err(message, comma_tok.loc, here!()));
                        }
                    }

                    let end_loc = self.pop().unwrap().loc;
                    let func_and_params = self.ast.add_exprs(func_and_params);

                    let kind = ExprKind::Call { func_and_params };
                    operand = self.e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::PlusPlus => {
                    let kind = ExprKind::UnaryOp(UnaryOp::PostIncr, self.ast.add_expr(operand));
                    let loc = l_from(start_loc, self.pop().unwrap().loc);
                    operand = self.e(kind, loc);
                }
                TokenKind::DashDash => {
                    let kind = ExprKind::UnaryOp(UnaryOp::PostDecr, self.ast.add_expr(operand));
                    let loc = l_from(start_loc, self.pop().unwrap().loc);
                    operand = self.e(kind, loc);
                }
                TokenKind::LBracket => {
                    let lbracket = self.pop().unwrap();
                    self.eat_newline();
                    let index = self.parse_expr()?;
                    self.eat_newline();
                    let rbracket =
                        self.expect_tok(TokenKind::RBracket, "expected a closing ']' bracket")?;

                    let loc = l_from(start_loc, rbracket.loc);

                    let kind = ExprKind::BinOp(BinOp::Index, self.ast.add_pair(operand, index));
                    operand = self.e(kind, loc);
                }
                TokenKind::Dot => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let ident_tok = self.pop_err()?;
                    if let TokenKind::Ident(member) = ident_tok.kind {
                        let kind = ExprKind::Member {
                            base: self.ast.add_expr(operand),
                            member,
                        };
                        operand = self.e(kind, l_from(start_loc, ident_tok.loc));

                        continue;
                    }

                    let message = "expected this to be an identifier";
                    return Err(self.err(message, ident_tok.loc, here!()));
                }
                _ => return Ok(operand),
            }
        }

        return Ok(operand);
    }

    pub fn parse_atom(&mut self) -> Result<Expr, Error> {
        let tok = self.pop_err()?;
        match tok.kind {
            TokenKind::Null => {
                return Ok(self.e(ExprKind::Null, tok.loc));
            }
            TokenKind::Ident(i) => {
                return Ok(self.e(ExprKind::Ident(i), tok.loc));
            }
            TokenKind::UxLit(num) => {
                return Ok(self.e(ExprKind::Ux(num), tok.loc));
            }
            TokenKind::StringLit(string) => {
                let mut string = string.as_str().to_string();
                self.eat_newline();
                let mut end_loc = tok.loc;
                while let TokenKind::StringLit(tstr) = self.peek_err()?.kind {
                    string.push_str(tstr.as_str());
                    end_loc = l_from(end_loc, self.pop().unwrap().loc);
                    self.eat_newline();
                }

                let kind = ExprKind::StringLit(self.ast.add_str(string));
                return Ok(self.e(kind, l_from(tok.loc, end_loc)));
            }
            TokenKind::New => {
                self.eat_newline();
                let (ty, ty_loc) = self.parse_type()?;
                let (loc, ty) = (l_from(tok.loc, ty_loc), self.ast.add_ty(ty));
                return Ok(self.e(ExprKind::New(ty), loc));
            }
            TokenKind::Struct => {
                let start = tok.loc;
                match self.peek_err()?.kind {
                    TokenKind::LBrace => {
                        self.pop().unwrap();

                        let stmts = self.parse_stmts()?;
                        let tok = self.expect_tok(TokenKind::RBrace, "expected '}' token")?;

                        let stmts = self.ast.add_stmts(stmts);
                        return Ok(self.e(ExprKind::Struct(stmts), l_from(start, tok.loc)));
                    }
                    _ => {
                        let (ty, ty_loc) = self.parse_type()?;
                        let (loc, ty) = (l_from(tok.loc, ty_loc), self.ast.add_ty(ty));
                        return Ok(self.e(ExprKind::UnitStruct(ty), loc));
                    }
                }
            }
            TokenKind::LBracket => {
                let start_loc = tok.loc;
                self.eat_newline();
                let mut expr = self.parse_expr()?;
                self.eat_newline();
                let mut expr_list = Vec::new();
                while self.peek_err()?.kind == TokenKind::Comma {
                    expr_list.push(expr);
                    self.pop().unwrap();
                    self.eat_newline();
                    expr = self.parse_expr()?;
                    self.eat_newline();
                }

                let end = self.expect_tok(TokenKind::RBracket, "expected a ']' here")?;
                expr_list.push(expr);

                let kind = ExprKind::List {
                    values: self.ast.add_exprs(expr_list),
                };
                return Ok(self.e(kind, l_from(start_loc, end.loc)));
            }
            TokenKind::LParen => {
                self.eat_newline();
                let (tok, mut params_len) = (self.peek_err()?, 0u16);
                let params = match tok.kind {
                    TokenKind::Ident(_) => {
                        if !self.is_decl() {
                            let expr = self.parse_expr()?;
                            self.eat_newline();

                            let end = self.expect_tok(TokenKind::RParen, "expected a ')' here")?;
                            return Ok(expr);
                        }

                        let mut params = Vec::new();
                        loop {
                            self.eat_newline();
                            params.push(self.expect_decl()?);
                            self.eat_newline();
                            params_len += 1;

                            let tok = self.pop_err()?;

                            if tok.kind == TokenKind::RParen {
                                break;
                            }

                            if tok.kind != TokenKind::Comma {
                                return Err(self.err("expected ',' or ')' here", tok.loc, here!()));
                            }
                        }

                        params
                    }
                    TokenKind::RParen => {
                        self.pop().unwrap();
                        Vec::new()
                    }
                    _ => {
                        let expr = self.parse_expr()?;

                        let end = self.expect_tok(TokenKind::RParen, "expected a ')' here")?;
                        return Ok(expr);
                    }
                };

                self.eat_newline();
                let ty = if self.peek_err()?.kind != TokenKind::Arrow {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                self.eat_newline();
                self.expect_tok(TokenKind::Arrow, "expected a '=>' here")?;

                self.eat_newline();
                let expr = self.parse_expr()?;

                let params = self.ast.add_decls(params).start;
                let kind = if let ExprKind::Block { stmts: body } = expr.kind {
                    ExprKind::Function {
                        params,
                        params_len,
                        body,
                    }
                } else {
                    let (loc, expr) = (expr.loc, self.ast.add_expr(expr));
                    let body = self.ast.add_stmts(vec![Stmt {
                        kind: StmtKind::RetVal(expr),
                        loc,
                    }]);

                    ExprKind::Function {
                        params,
                        params_len,
                        body,
                    }
                };

                return Ok(self.e(kind, l_from(tok.loc, expr.loc)));
            }
            TokenKind::LBrace => {
                let start = tok.loc;

                let stmts = self.parse_stmts()?;
                let tok = self.expect_tok(TokenKind::RBrace, "expected '}' token")?;

                let stmts = self.ast.add_stmts(stmts);
                return Ok(self.e(ExprKind::Block { stmts }, l_from(start, tok.loc)));
            }
            _ => return Err(self.err("unexpected token here", tok.loc, here!())),
        }
    }

    #[inline]
    pub fn parse_stmts(&mut self) -> Result<Vec<Stmt>, Error> {
        let (mut stmts, mut decls, mut decl_stmts) = (Vec::new(), Vec::new(), Vec::new());
        self.eat_newline();
        while let Some(tok) = self.peek() {
            if tok.kind == TokenKind::RBrace {
                break;
            }

            if !self.is_decl() {
                stmts.push(self.parse_stmt()?);
                self.eat_newline();
                continue;
            }

            let decl = self.expect_decl()?;
            self.eat_line_ending();

            decls.push(decl);
            decl_stmts.push(stmts.len());
            stmts.push(Stmt {
                kind: StmtKind::Decl(DeclIdx::illegal()),
                loc: decl.loc,
            });
        }

        let decl_range = self.ast.add_decls(decls);
        let mut cur_decl_idx = decl_range.start;
        debug_assert_eq!(cur_decl_idx.add(decl_stmts.len() as u32), decl_range.end);

        for idx in decl_stmts {
            if let StmtKind::Decl(idx) = &mut stmts[idx].kind {
                *idx = cur_decl_idx;
                cur_decl_idx = cur_decl_idx.add(1);
                continue;
            }

            unreachable!("{:?}", stmts[idx]);
        }

        return Ok(stmts);
    }

    pub fn parse_type(&mut self) -> Result<(Type, CodeLoc), Error> {
        let mut modifiers = Vec::new();
        let mut tok = self.pop_err()?;
        let mut loc = tok.loc;
        loop {
            match tok.kind {
                TokenKind::Star => modifiers.push(TypeModifier::Pointer),
                TokenKind::DotDotDot => modifiers.push(TypeModifier::Varargs),
                TokenKind::LBracket => {
                    let tok = self.peek_err()?;
                    match tok.kind {
                        TokenKind::RBracket => modifiers.push(TypeModifier::VarArray),
                        TokenKind::DotDotDot => {
                            if let Some(tok) = self.peek_2() {
                                if tok.kind == TokenKind::RBracket {
                                    self.pop().unwrap();
                                    modifiers.push(TypeModifier::Slice);
                                } else {
                                    let expr = self.parse_expr()?;
                                    modifiers.push(TypeModifier::Array(self.ast.add_expr(expr)));
                                }
                            } else {
                                let expr = self.parse_expr()?;
                                modifiers.push(TypeModifier::Array(self.ast.add_expr(expr)));
                            }
                        }
                        _ => {
                            let expr = self.parse_expr()?;
                            modifiers.push(TypeModifier::Array(self.ast.add_expr(expr)));
                        }
                    }

                    self.expect_tok(TokenKind::RBracket, "expected closing ']' token")?;
                }

                TokenKind::String => {
                    let modifiers = self.ast.add_ty_mods(modifiers);
                    let ty = Type {
                        modifiers,
                        base: TypeBase::String,
                    };

                    return Ok((ty, loc));
                }
                TokenKind::U64 => {
                    let modifiers = self.ast.add_ty_mods(modifiers);
                    let ty = Type {
                        modifiers,
                        base: TypeBase::U64,
                    };

                    return Ok((ty, loc));
                }
                TokenKind::Any => {
                    let modifiers = self.ast.add_ty_mods(modifiers);
                    let ty = Type {
                        modifiers,
                        base: TypeBase::Any,
                    };

                    return Ok((ty, loc));
                }
                TokenKind::Ident(id) => {
                    let modifiers = self.ast.add_ty_mods(modifiers);
                    let ty = Type {
                        modifiers,
                        base: TypeBase::Named(id),
                    };

                    return Ok((ty, loc));
                }

                kind => {
                    return Err(self.err("unexpected token while parsing type", tok.loc, here!()))
                }
            }

            self.eat_newline();
            tok = self.pop_err()?;
            loc = l_from(loc, tok.loc);
        }
    }

    pub fn peek_err(&mut self) -> Result<Token<'data>, Error> {
        return self.peek().ok_or_else(|| Error {
            info: "expected another token after this one",
            message: None,
            loc: self.tokens[self.current - 1].loc,
            file: self.ast.file,
            #[cfg(debug_assertions)]
            compiler_loc: here!(),
        });
    }

    pub fn pop_err(&mut self) -> Result<Token<'data>, Error> {
        let tok = self.peek_err()?;
        self.current += 1;

        return Ok(tok);
    }

    pub fn eat_newline(&mut self) {
        while let Some(Token {
            kind: TokenKind::Newline,
            ..
        }) = self.peek()
        {
            self.current += 1;
        }
    }

    pub fn eat_line_ending(&mut self) {
        while let Some(tok) = self.peek() {
            if tok.kind != TokenKind::Semicolon && tok.kind != TokenKind::Newline {
                break;
            }

            self.current += 1;
        }
    }

    pub fn expect_tok(
        &mut self,
        kind: TokenKind,
        err: &'static str,
    ) -> Result<Token<'data>, Error> {
        let tok = self.pop_err()?;
        if tok.kind == kind {
            return Ok(tok);
        }

        return Err(self.err(err, tok.loc, here!()));
    }

    pub fn peek(&mut self) -> Option<Token<'data>> {
        return self.tokens.get(self.current).map(|a| *a);
    }

    pub fn peek_2(&mut self) -> Option<Token<'data>> {
        return self.tokens.get(self.current + 1).map(|a| *a);
    }

    pub fn pop(&mut self) -> Option<Token<'data>> {
        let tok = self.peek()?;
        self.current += 1;

        return Some(tok);
    }

    pub fn err(&self, info: &'static str, loc: CodeLoc, cloc: CompilerLoc) -> Error {
        return Error {
            info,
            message: None,
            loc,
            file: self.ast.file,
            #[cfg(debug_assertions)]
            compiler_loc: cloc,
        };
    }
}

// ---------------------------------------------------------------------------
//
//                                  LEXING
//
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind<'a> {
    Ident(u32),
    UxLit(u64),
    StringLit(&'a IStr),
    CharLit([u8; 4]),
    Null,

    Error(&'a IStr),

    Struct,
    String,
    U64,
    S64,
    Any,

    If,
    Else,
    For,
    While,
    Break,
    Continue,
    Return,
    In,
    New,

    Dot,
    DotDotDot,
    Bang,
    Question,
    Tilde,
    Star,
    Slash,
    Plus,
    Dash,
    Percent,
    PlusPlus,
    DashDash,
    Colon,
    Arrow,

    Eq,
    EqEq,
    Neq,
    Leq,
    Lt,
    LtLt, // <<
    Geq,
    Gt,
    GtGt, // >>
    Amp,
    AmpAmp,
    Line,     // |
    LineLine, // ||
    Caret,
    AmpEq,
    LineEq,
    CaretEq,
    PlusEq,
    DashEq,
    SlashEq,
    StarEq,
    PercentEq,
    LtLtEq,
    GtGtEq,

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Semicolon,
    Comma,
    Newline,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub loc: CodeLoc,
}

pub struct Lexer<'a> {
    pub buckets: BucketListFactory,
    pub symbols: Symbols,
    pub marker: PhantomData<&'a mut u8>,
}

pub struct Lexing<'input, 'lexer, 'output> {
    pub lexer: &'lexer mut Lexer<'output>,
    pub data: &'input [u8],
    pub begin: usize,
    pub current: usize,
    marker: PhantomData<&'output mut u8>,
}

impl<'a> Drop for Lexer<'a> {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

lazy_static! {
    pub static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenKind<'static>> = {
        let mut set = HashMap::new();

        set.insert("break", TokenKind::Break);
        set.insert("continue", TokenKind::Continue);
        set.insert("for", TokenKind::For);
        set.insert("while", TokenKind::While);
        set.insert("if", TokenKind::If);
        set.insert("else", TokenKind::Else);
        set.insert("return", TokenKind::Return);
        set.insert("in", TokenKind::In);

        set.insert("struct", TokenKind::Struct);
        set.insert("string", TokenKind::String);
        set.insert("u64", TokenKind::U64);
        set.insert("s64", TokenKind::S64);
        set.insert("any", TokenKind::Any);

        set.insert("null", TokenKind::Null);

        set
    };
}

impl<'a> Lexer<'a> {
    pub fn new() -> Self {
        Self {
            buckets: BucketListFactory::new(),
            symbols: Symbols::new(),
            marker: PhantomData,
        }
    }

    pub fn lex<'input, 'lexer>(&'lexer mut self, file: &'input str) -> Lexing<'input, 'lexer, 'a> {
        Lexing {
            lexer: self,
            data: file.as_bytes(),
            begin: 0,
            current: 0,
            marker: PhantomData,
        }
    }
}

impl<'input, 'lexer, 'output> Iterator for Lexing<'input, 'lexer, 'output> {
    type Item = Token<'output>;

    fn next(&mut self) -> Option<Token<'output>> {
        macro_rules! ret {
            ($arg1:expr) => {{
                return Some(Token {
                    kind: $arg1,
                    loc: l(self.begin, self.current),
                });
            }};
        }

        macro_rules! err_ret {
            ($arg1:expr) => {{
                ret!(TokenKind::Error(self.lexer.buckets.add_i_str($arg1)));
            }};
            (@RES, $arg1:expr) => {{
                match $arg1 {
                    Ok(i) => i,
                    Err(e) => ret!(TokenKind::Error(self.lexer.buckets.add_i_str(e))),
                }
            }};
        }

        macro_rules! incr_ret {
            ($arg1:expr) => {{
                self.current += 1;
                ret!($arg1);
            }};
        }

        match self.kill_whitespace() {
            Err(s) => err_ret!(s),
            Ok(true) => ret!(TokenKind::Newline),
            Ok(false) => {}
        }

        if self.current == self.data.len() {
            return None;
        }

        self.begin = self.current;
        self.current += 1;

        match self.data[self.begin] {
            x if (x >= b'A' && x <= b'Z') || (x >= b'a' && x <= b'z') || x == b'_' => {
                while self.peek_check(is_ident_char) {
                    self.current += 1;
                }

                let word =
                    unsafe { core::str::from_utf8_unchecked(&self.data[self.begin..self.current]) };
                if let Some(kind) = RESERVED_KEYWORDS.get(word) {
                    ret!(*kind);
                }

                let id = self.lexer.symbols.add_str(word);
                ret!(TokenKind::Ident(id));
            }

            // TODO parse numbers
            b @ b'0'
            | b @ b'1'
            | b @ b'2'
            | b @ b'3'
            | b @ b'4'
            | b @ b'5'
            | b @ b'6'
            | b @ b'7'
            | b @ b'8'
            | b @ b'9' => {
                let mut num: u64 = (b - b'0') as u64;
                let is_numeric = |b: u8| b >= b'0' && b <= b'9';
                while self.peek_check(is_numeric) {
                    num = match num.checked_mul(10) {
                        Some(num) => num,
                        None => {
                            while self.peek_check(is_numeric) {
                                self.current += 1;
                            }

                            err_ret!("number is too big");
                        }
                    };

                    num = match num.checked_add((self.data[self.current] - b'0') as u64) {
                        Some(num) => num,
                        None => {
                            while self.peek_check(is_numeric) {
                                self.current += 1;
                            }

                            err_ret!("number is too big");
                        }
                    };

                    self.current += 1;
                }

                ret!(TokenKind::UxLit(num));
            }

            b'\"' => {
                let mut cur = err_ret!(@RES, self.lex_character(b'\"'));
                let mut chars = Vec::new();
                while cur != 0 {
                    chars.push(cur);
                    cur = err_ret!(@RES, self.lex_character(b'\"'));
                }

                let string = unsafe { core::str::from_utf8_unchecked(&chars) };
                let string = self.lexer.buckets.add_i_str(string);
                ret!(TokenKind::StringLit(string));
            }

            b'{' => ret!(TokenKind::LBrace),
            b'}' => ret!(TokenKind::RBrace),
            b'(' => ret!(TokenKind::LParen),
            b')' => ret!(TokenKind::RParen),
            b'[' => ret!(TokenKind::LBracket),
            b']' => ret!(TokenKind::RBracket),
            b'~' => ret!(TokenKind::Tilde),
            b';' => ret!(TokenKind::Semicolon),
            b':' => ret!(TokenKind::Colon),
            b',' => ret!(TokenKind::Comma),
            b'?' => ret!(TokenKind::Question),
            b'#' => unimplemented!("compile directives aren't implemented yet"),

            b'.' => {
                if self.peek_eq(b'.') {
                    self.current += 1;
                    if self.peek_eq(b'.') {
                        incr_ret!(TokenKind::DotDotDot);
                    }

                    err_ret!("'..' is invalid. Is '...' what you meant?");
                }

                ret!(TokenKind::Dot);
            }
            b'+' => {
                if self.peek_eq(b'+') {
                    incr_ret!(TokenKind::PlusPlus);
                } else if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::PlusEq);
                } else {
                    ret!(TokenKind::Plus);
                }
            }
            b'-' => {
                if self.peek_eq(b'-') {
                    incr_ret!(TokenKind::DashDash);
                } else if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::DashEq);
                } else {
                    ret!(TokenKind::Dash);
                }
            }
            b'/' => {
                if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::SlashEq);
                } else {
                    ret!(TokenKind::Slash);
                }
            }
            b'*' => {
                if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::StarEq);
                } else {
                    ret!(TokenKind::Star);
                }
            }
            b'%' => {
                if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::PercentEq);
                } else {
                    ret!(TokenKind::Percent);
                }
            }
            b'>' => {
                if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::Geq);
                } else if self.peek_eq(b'>') {
                    self.current += 1;
                    if self.peek_eq(b'=') {
                        incr_ret!(TokenKind::GtGtEq);
                    }
                    ret!(TokenKind::GtGt);
                } else {
                    ret!(TokenKind::Gt);
                }
            }
            b'<' => {
                if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::Leq);
                } else if self.peek_eq(b'<') {
                    self.current += 1;
                    if self.peek_eq(b'=') {
                        incr_ret!(TokenKind::LtLtEq);
                    }
                    ret!(TokenKind::LtLt);
                } else {
                    ret!(TokenKind::Lt);
                }
            }
            b'!' => {
                if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::Neq);
                } else {
                    ret!(TokenKind::Bang);
                }
            }
            b'=' => {
                if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::EqEq);
                } else if self.peek_eq(b'>') {
                    incr_ret!(TokenKind::Arrow);
                } else {
                    ret!(TokenKind::Eq);
                }
            }
            b'|' => {
                if self.peek_eq(b'|') {
                    incr_ret!(TokenKind::LineLine);
                } else if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::LineEq);
                } else {
                    ret!(TokenKind::Line);
                }
            }
            b'&' => {
                if self.peek_eq(b'&') {
                    incr_ret!(TokenKind::AmpAmp);
                } else if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::AmpEq);
                } else {
                    ret!(TokenKind::Amp);
                }
            }
            b'^' => {
                if self.peek_eq(b'=') {
                    incr_ret!(TokenKind::CaretEq);
                } else {
                    ret!(TokenKind::Caret);
                }
            }

            x => {
                err_ret!("invalid token");
            }
        }
    }
}

const WHITESPACE: [u8; 2] = [b' ', b'\t'];
const CRLF: [u8; 2] = [b'\r', b'\n'];

impl<'input, 'lexer, 'output> Lexing<'input, 'lexer, 'output> {
    pub fn kill_whitespace(&mut self) -> Result<bool, &'static str> {
        let mut newlined = false;
        self.begin = self.current;

        loop {
            while self.peek_eqs(&WHITESPACE) {
                self.current += 1;
            }

            if self.peek_eq_series(&[b'/', b'/']) {
                self.current += 2;
                loop {
                    if self.current == self.data.len() {
                        return Ok(newlined);
                    }

                    if self.peek_eq(b'\n') || self.peek_eq_series(&CRLF) {
                        newlined = true;
                        break;
                    }
                    self.current += 1;
                }
            } else if self.peek_eq_series(&[b'/', b'*']) {
                self.current += 2;
                loop {
                    if self.current == self.data.len() {
                        return Err("block comment still open when file ends");
                    }

                    if self.peek_eq_series(&[b'*', b'/']) {
                        break;
                    }

                    if self.peek_eq(b'\n') || self.peek_eq_series(&CRLF) {
                        newlined = true;
                        break;
                    }

                    self.current += 1;
                }

                self.current += 2;
                continue;
            }

            if self.peek_eq(b'\n') {
                newlined = true;
                self.current += 1;
            } else if self.peek_eq_series(&CRLF) {
                newlined = true;
                self.current += 2;
            } else {
                break;
            }
        }

        return Ok(newlined);
    }
    #[inline]
    pub fn expect(&mut self) -> Result<u8, &'static str> {
        if self.current == self.data.len() {
            return Err("unexpected end of file");
        }

        let cur = self.current;
        self.current += 1;
        return Ok(self.data[cur]);
    }

    #[inline]
    pub fn peek_expect(&self) -> Result<u8, &'static str> {
        if self.current == self.data.len() {
            return Err("unexpected end of file");
        }

        return Ok(self.data[self.current]);
    }

    #[inline]
    pub fn peek_check(&self, checker: impl Fn(u8) -> bool) -> bool {
        if self.current >= self.data.len() {
            return false;
        }

        return checker(self.data[self.current]);
    }

    #[inline]
    pub fn peek_eq(&self, byte: u8) -> bool {
        if self.current >= self.data.len() {
            return false;
        }

        return self.data[self.current] == byte;
    }

    pub fn peek_neq_series(&self, bytes: &[u8]) -> bool {
        let byte_len = bytes.len();
        if self.current + bytes.len() > self.data.len() {
            return false;
        }

        let eq_slice = &self.data[(self.current)..(self.current + byte_len)];
        return eq_slice != bytes;
    }

    pub fn peek_eq_series(&self, bytes: &[u8]) -> bool {
        let byte_len = bytes.len();
        if self.current + bytes.len() > self.data.len() {
            return false;
        }

        let eq_slice = &self.data[(self.current)..(self.current + byte_len)];
        return eq_slice == bytes;
    }

    #[inline]
    pub fn peek_neq(&self, byte: u8) -> bool {
        if self.current >= self.data.len() {
            return false;
        }

        return self.data[self.current] != byte;
    }

    #[inline]
    pub fn peek_neqs(&self, bytes: &[u8]) -> bool {
        if self.current >= self.data.len() {
            return false;
        }

        for byte in bytes {
            if self.data[self.current] == *byte {
                return false;
            }
        }

        return true;
    }

    #[inline]
    pub fn peek_eqs(&self, bytes: &[u8]) -> bool {
        if self.current >= self.data.len() {
            return false;
        }

        for byte in bytes {
            if self.data[self.current] == *byte {
                return true;
            }
        }

        return false;
    }

    pub fn lex_character(&mut self, surround: u8) -> Result<u8, &'static str> {
        loop {
            let cur_b = self.expect()?;
            let cur: char = cur_b.into();

            if !cur.is_ascii() {
                return Err("character is not valid ascii");
            }

            if cur_b == surround {
                return Ok(0);
            }

            if cur_b == b'\n' || cur_b == b'\r' {
                if surround == b'\"' {
                    return Err("invalid character found when parsing string literal");
                } else {
                    return Err("invalid character found when parsing character literal");
                }
            }

            if cur_b != b'\\' {
                return Ok(cur_b);
            }

            match self.expect()? {
                b'n' => return Ok(b'\n'),
                b't' => return Ok(b'\t'),
                b'\'' => return Ok(b'\''),
                b'"' => return Ok(b'"'),

                // \nnn where each 'n' is an octal digit
                x @ b'0'..=b'7' => {
                    let mut c = x - b'0';
                    if !self.peek_check(|c| c >= b'0' && c <= b'7') {
                        return Ok(c);
                    }

                    c *= 8;
                    c += self.data[self.current] - b'0';
                    self.current += 1;

                    if !self.peek_check(|c| c >= b'0' && c <= b'7') {
                        return Ok(c);
                    }

                    c *= 8;
                    c += self.data[self.current] - b'0';
                    self.current += 1;

                    return Ok(c);
                }

                b'\n' => continue,
                b'\r' => {
                    if self.peek_eq(b'\n') {
                        self.current += 1;
                        continue;
                    } else {
                        return Err("encoding of the file is probably messed up");
                    }
                }

                _ => return Err("invalid escape sequence"),
            }
        }
    }
}

pub fn is_ident_char(cur: u8) -> bool {
    (cur >= b'a' && cur <= b'z')
        || (cur >= b'A' && cur <= b'Z')
        || cur == b'_'
        || (cur >= b'0' && cur <= b'9')
}
