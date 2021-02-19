use crate::buckets::*;
use crate::filedb::*;
use crate::util::*;
use core::marker::PhantomData;

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
