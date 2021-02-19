use crate::ast::*;
use crate::filedb::*;
use crate::lexer::*;
use crate::util::*;

pub fn parse_file(lexer: &mut Lexer, id: u32, file: &str) -> Result<Ast, Error> {
    let mut parser = Parser::new(id, lexer.lex(file));
    parser.eat_newline();
    // let toks: Vec<_> = parser.tokens.iter().map(|a| a.kind).collect();
    // println!("{:?}", toks);

    let mut stmts = Vec::new();
    while parser.current < parser.tokens.len() {
        let stmt = parser.parse_stmt()?;

        stmts.push(stmt);
    }

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

                let iter = e(ExprKind::Ident(BuiltinSymbol::It as u32), first.loc);

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

            TokenKind::Ident(id) => {
                if !self.is_decl() {
                    let expr = self.parse_expr()?;
                    self.eat_line_ending();

                    return Ok(Stmt {
                        kind: StmtKind::Expr(expr.kind),
                        loc: expr.loc,
                    });
                }

                let (decl, loc) = self.expect_decl()?;
                self.eat_line_ending();

                return Ok(Stmt {
                    kind: StmtKind::Decl(decl),
                    loc,
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

                return Ok(Stmt {
                    kind: StmtKind::Expr(expr.kind),
                    loc: expr.loc,
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

        return tok.kind == TokenKind::Comma || tok.kind == TokenKind::Colon;
    }

    pub fn expect_decl(&mut self) -> Result<(Decl, CodeLoc), Error> {
        let tok = self.pop_err()?;
        let (mut idents, mut loc) = match tok.kind {
            TokenKind::Ident(sym) => (vec![sym], tok.loc),
            _ => return Err(self.err("expected identifier to begin declaration", tok.loc)),
        };

        let mut tok = self.pop_err()?;
        while tok.kind == TokenKind::Comma {
            self.eat_newline();
            let ident_tok = self.pop_err()?;
            if let TokenKind::Ident(id) = ident_tok.kind {
                idents.push(id);
                loc = l_from(loc, ident_tok.loc);
                tok = self.pop_err()?;
                continue;
            }

            return Err(self.err("expected this to be an identifier", ident_tok.loc));
        }

        if tok.kind != TokenKind::Colon {
            return Err(self.err("expected this to be a ':' token", tok.loc));
        }

        self.eat_newline();
        let tok = self.peek_err()?;
        let ty = if tok.kind != TokenKind::Eq {
            let ty = self.parse_type()?;
            let (loc, ty) = (l_from(loc, ty.loc), self.ast.add_ty(ty));
            self.eat_newline();
            if let Some(tok) = self.peek() {
                if tok.kind != TokenKind::Eq {
                    let (ty, expr, idents) = (Some(ty), None, self.ast.add_idents(idents));
                    return Ok((Decl { idents, ty, expr }, loc));
                }
            }

            Some(ty)
        } else {
            None
        };

        let tok = self.expect_tok(TokenKind::Eq, "expected an '=' token")?;
        self.eat_newline();
        let expr = self.parse_expr()?;
        let (expr_loc, expr) = (expr.loc, Some(self.ast.add_expr(expr)));

        let idents = self.ast.add_idents(idents);
        return Ok((Decl { idents, ty, expr }, l_from(loc, expr_loc)));
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
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                return Ok(e(ExprKind::Assign(left, right), loc));
            }
            TokenKind::PlusEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));

                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::Add,
                };

                return Ok(e(kind, loc));
            }
            TokenKind::DashEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::Sub,
                };

                return Ok(e(kind, loc));
            }
            TokenKind::StarEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::Mul,
                };

                return Ok(e(kind, loc));
            }
            TokenKind::SlashEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::Div,
                };

                return Ok(e(kind, loc));
            }
            TokenKind::PercentEq => {
                self.pop().unwrap();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::Mod,
                };
                return Ok(e(kind, loc));
            }
            TokenKind::LtLtEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::LShift,
                };

                return Ok(e(kind, loc));
            }

            TokenKind::GtGtEq => {
                self.pop().unwrap();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::RShift,
                };

                return Ok(e(kind, loc));
            }

            TokenKind::AmpEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::BitAnd,
                };

                return Ok(e(kind, loc));
            }
            TokenKind::CaretEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::BitXor,
                };

                return Ok(e(kind, loc));
            }
            TokenKind::LineEq => {
                self.pop().unwrap();
                self.eat_newline();
                let right = self.parse_assign()?;
                let loc = l_from(left.loc, right.loc);
                let (right, left) = (self.ast.add_expr(right), self.ast.add_expr(left));
                let kind = ExprKind::MutAssign {
                    target: left,
                    value: right,
                    op: BinOp::BitOr,
                };

                return Ok(e(kind, loc));
            }
            _ => {
                return Ok(left);
            }
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
            return Err(self.err(
                "expected ':' token, got something else instead",
                colon_tok.loc,
            ));
        }

        self.eat_newline();
        let if_false = self.parse_bool_or()?;

        let loc = l_from(condition.loc, if_false.loc);
        let condition = self.ast.add_expr(condition);
        let if_true = self.ast.add_expr(if_true);
        let if_false = self.ast.add_expr(if_false);

        let kind = ExprKind::Ternary {
            condition,
            if_true,
            if_false,
        };

        return Ok(e(kind, loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::BoolOr, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::BoolAnd, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::BitOr, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::BitXor, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::BitAnd, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Eq, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Neq => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_comparison()?;
                    let end_loc = right.loc;
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Neq, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Lt, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Leq => {
                    self.pop().unwrap();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Leq, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Gt => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Gt, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Geq => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Geq, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::RShift, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::LtLt => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_add()?;
                    let end_loc = right.loc;
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::LShift, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Add, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Dash => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_multiply()?;
                    let end_loc = right.loc;
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Sub, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Div, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Star => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_prefix()?;
                    let end_loc = right.loc;
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Mul, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::Percent => {
                    self.pop().unwrap();
                    self.eat_newline();

                    let right = self.parse_prefix()?;
                    let end_loc = right.loc;
                    let left = self.ast.add_expr(expr);
                    let right = self.ast.add_expr(right);

                    let kind = ExprKind::BinOp(BinOp::Mod, left, right);
                    expr = e(kind, l_from(start_loc, end_loc));
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
                return Ok(e(ExprKind::UnaryOp(UnaryOp::Ref, target), loc));
            }
            TokenKind::Star => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(e(ExprKind::UnaryOp(UnaryOp::Deref, target), loc));
            }

            TokenKind::Bang => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(e(ExprKind::UnaryOp(UnaryOp::BoolNot, target), loc));
            }
            TokenKind::Tilde => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(e(ExprKind::UnaryOp(UnaryOp::BitNot, target), loc));
            }
            TokenKind::Dash => {
                self.pop().unwrap();
                self.eat_newline();

                let target = self.parse_prefix()?;
                let (loc, target) = (l_from(tok.loc, target.loc), self.ast.add_expr(target));
                return Ok(e(ExprKind::UnaryOp(UnaryOp::Neg, target), loc));
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

        let range = e(ExprKind::Range(Some(start), Some(end)), loc);

        let tok = match self.peek() {
            Some(tok) => tok,
            None => return Ok(range),
        };

        if tok.kind == TokenKind::DotDotDot {
            return Err(self.err("'...' operator cannot be chained", loc));
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

                    let mut params = Vec::new();
                    let rparen_tok = self.peek_err()?;

                    if rparen_tok.kind != TokenKind::RParen {
                        let param = self.parse_expr()?;
                        params.push(param);
                        self.eat_newline();
                        let mut comma_tok = self.peek_err()?;

                        while comma_tok.kind == TokenKind::Comma {
                            self.pop().unwrap();
                            self.eat_newline();
                            params.push(self.parse_expr()?);
                            self.eat_newline();
                            comma_tok = self.peek_err()?;
                        }

                        if comma_tok.kind != TokenKind::RParen {
                            return Err(self.err(
                                "unexpected token when parsing end of function call",
                                params.pop().unwrap().loc,
                            ));
                        }
                    }

                    let end_loc = self.pop().unwrap().loc;
                    let params = self.ast.add_exprs(params);

                    let kind = ExprKind::Call {
                        function: self.ast.add_expr(operand),
                        params,
                    };
                    operand = e(kind, l_from(start_loc, end_loc));
                }
                TokenKind::PlusPlus => {
                    let kind = ExprKind::UnaryOp(UnaryOp::PostIncr, self.ast.add_expr(operand));
                    operand = e(kind, l_from(start_loc, self.pop().unwrap().loc));
                }
                TokenKind::DashDash => {
                    let kind = ExprKind::UnaryOp(UnaryOp::PostDecr, self.ast.add_expr(operand));
                    operand = e(kind, l_from(start_loc, self.pop().unwrap().loc));
                }
                TokenKind::LBracket => {
                    let lbracket = self.pop().unwrap();
                    self.eat_newline();
                    let index = self.parse_expr()?;
                    self.eat_newline();
                    let rbracket =
                        self.expect_tok(TokenKind::RBracket, "expected a closing ']' bracket")?;

                    let loc = l_from(start_loc, rbracket.loc);

                    let kind = ExprKind::BinOp(
                        BinOp::Index,
                        self.ast.add_expr(operand),
                        self.ast.add_expr(index),
                    );
                    operand = e(kind, loc);
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
                        operand = e(kind, l_from(start_loc, ident_tok.loc));

                        continue;
                    }

                    return Err(self.err("expected this to be an identifier", ident_tok.loc));
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
                return Ok(e(ExprKind::Null, tok.loc));
            }
            TokenKind::Ident(i) => {
                return Ok(e(ExprKind::Ident(i), tok.loc));
            }
            TokenKind::UxLit(num) => {
                return Ok(e(ExprKind::Ux(num), tok.loc));
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
                return Ok(e(kind, l_from(tok.loc, end_loc)));
            }
            TokenKind::New => {
                self.eat_newline();
                let ty = self.parse_type()?;
                let (loc, ty) = (l_from(tok.loc, ty.loc), self.ast.add_ty(ty));
                return Ok(e(ExprKind::New(ty), loc));
            }
            TokenKind::Struct => {
                let start = tok.loc;
                match self.peek_err()?.kind {
                    TokenKind::LBrace => {
                        self.pop().unwrap();
                        let mut stmts = Vec::new();
                        self.eat_newline();
                        let mut tok = self.peek_err()?;
                        while tok.kind != TokenKind::RBrace {
                            let stmt = self.parse_stmt()?;
                            stmts.push(stmt);

                            tok = self.peek_err()?;
                        }

                        let tok = self.expect_tok(TokenKind::RBrace, "expected '}' token")?;

                        let stmts = self.ast.add_stmts(stmts);
                        return Ok(e(ExprKind::Struct(stmts), l_from(start, tok.loc)));
                    }
                    _ => {
                        let ty = self.parse_type()?;
                        let (loc, ty) = (l_from(tok.loc, ty.loc), self.ast.add_ty(ty));
                        return Ok(e(ExprKind::UnitStruct(ty), loc));
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
                    ty: None,
                    values: self.ast.add_exprs(expr_list),
                };
                return Ok(e(kind, l_from(start_loc, end.loc)));
            }
            TokenKind::LParen => {
                self.eat_newline();
                let tok = self.peek_err()?;
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

                            let tok = self.pop_err()?;

                            if tok.kind == TokenKind::RParen {
                                break;
                            }

                            if tok.kind != TokenKind::Comma {
                                return Err(self.err("expected ',' or ')' here", tok.loc));
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

                let kind = ExprKind::Function {
                    params: self.ast.add_params(params),
                    body: self.ast.add_expr(expr),
                };
                return Ok(e(kind, l_from(tok.loc, expr.loc)));
            }
            TokenKind::LBrace => {
                let start = tok.loc;

                let mut stmts = Vec::new();
                self.eat_newline();
                let mut tok = self.peek_err()?;
                while tok.kind != TokenKind::RBrace {
                    let stmt = self.parse_stmt()?;
                    stmts.push(stmt);

                    tok = self.peek_err()?;
                }

                let tok = self.expect_tok(TokenKind::RBrace, "expected '}' token")?;

                let stmts = self.ast.add_stmts(stmts);
                return Ok(e(ExprKind::Block(stmts), l_from(start, tok.loc)));
            }
            _ => return Err(self.err("unexpected token here", tok.loc)),
        }
    }

    pub fn parse_type(&mut self) -> Result<Type, Error> {
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
                    return Ok(Type {
                        modifiers,
                        base: TypeBase::String,
                        loc,
                    });
                }
                TokenKind::U64 => {
                    let modifiers = self.ast.add_ty_mods(modifiers);
                    return Ok(Type {
                        modifiers,
                        base: TypeBase::U64,
                        loc,
                    });
                }
                TokenKind::Any => {
                    let modifiers = self.ast.add_ty_mods(modifiers);
                    return Ok(Type {
                        modifiers,
                        base: TypeBase::Any,
                        loc,
                    });
                }
                TokenKind::Ident(id) => {
                    let modifiers = self.ast.add_ty_mods(modifiers);
                    return Ok(Type {
                        modifiers,
                        base: TypeBase::Named(id),
                        loc,
                    });
                }

                kind => return Err(self.err("unexpected token while parsing type", tok.loc)),
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

        return Err(self.err(err, tok.loc));
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

    pub fn err(&self, info: &'static str, loc: CodeLoc) -> Error {
        return Error {
            info,
            message: None,
            loc,
            file: self.ast.file,
        };
    }
}
