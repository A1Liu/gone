use crate::buckets::*;
use crate::filedb::*;
use core::marker::PhantomData;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Ident(u32),
    LiteralOrError(&'static IStr),

    Struct,

    If,
    Else,
    For,
    Break,
    Continue,
    Return,

    Dot,
    DotDotDot,
    Arrow,
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
}

pub struct Token {
    pub kind: TokenKind,
}

pub struct Lexer<'a> {
    pub buckets: BucketListFactory,
    pub symbols: Symbols,
    pub marker: PhantomData<&'a mut u8>,
}

pub struct Lexing<'input, 'lexer, 'output> {
    pub symbols: &'lexer mut Symbols,
    pub data: &'input [u8],
    pub index: usize,
    marker: PhantomData<&'output mut u8>,
}

impl<'a> Drop for Lexer<'a> {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

impl<'a> Lexer<'a> {
    pub fn new() -> Self {
        Self {
            buckets: BucketListFactory::new(),
            symbols: Symbols::new(),
            marker: PhantomData,
        }
    }

    pub fn lex<'input, 'b>(&'b mut self, file: &'input str) -> Lexing<'input, 'b, 'a> {
        Lexing {
            symbols: &mut self.symbols,
            data: file.as_bytes(),
            index: 0,
            marker: PhantomData,
        }
    }
}
