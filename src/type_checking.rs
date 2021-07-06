use crate::ast::*;
use crate::buckets::*;
use std::collections::HashMap;

pub struct TypeTable {
    pub types: HashMap<u32, u32>, // type-id to definition
    pub next_id: u32,
}

pub struct CheckEnv {}

pub fn type_check(alloc: &impl Allocator<'static>, stmts: &[Stmt]) {}
