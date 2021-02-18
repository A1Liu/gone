use crate::ast::*;
use crate::buckets::*;
// use crate::lexer::*;
use crate::util::*;

pub struct Ast {
    pub buckets: BucketListFactory,
    pub file: u32,
    block: &'static [Statement<'static>],
}

impl Ast {
    pub fn new(
        buckets: BucketListFactory,
        file: u32,
        block: &'static [Statement<'static>],
    ) -> Self {
        Self {
            buckets,
            file,
            block,
        }
    }

    pub fn block<'a>(&'a self) -> &'a [Statement<'a>] {
        return self.block;
    }
}

pub fn parse_file(id: u32, file: &str) -> Ast {
    let buckets = BucketListFactory::new();

    let stmts = buckets.add_array(Vec::new());
    return Ast::new(buckets, id, stmts);
}

pub struct Parser {}

pub fn parse_stmt(buckets: &impl Allocator<'static>) -> Statement<'static> {
    unimplemented!()
}
