use crate::ast::*;
use crate::buckets::*;
use std::collections::HashMap;

pub struct TypeId(i64); // should be 64-bit

pub struct TypeTable {
    pub types: Vec<Type>, // type-id to definition
}

pub struct CheckEnv {}

pub fn type_check(alloc: &impl Allocator<'static>, stmts: &[Stmt]) {}
