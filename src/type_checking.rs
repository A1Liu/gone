use crate::ast::*;
use crate::buckets::*;
use crate::util::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct TypeId(u32);

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct GlobalId(u32);

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ValueId(u32);

#[derive(Debug, Clone, Copy)]
pub enum TypeName {
    Struct(&'static [Decl]),
    Tuple(&'static [Spanned<Type>]),
    Enum(&'static [Spanned<Type>]),
    Slice(&'static Spanned<Type>),
    Ident(Spanned<TypeId>),
    U64,
    String,
    Bool,
    Range,
}

#[derive(Debug, Clone, Copy)]
pub struct Type {
    pub name: TypeName,
    pub ptr: bool,
}

#[derive(Debug)]
pub struct GlobalTable {
    pub types: Vec<Type>,         // type-id to definition
    pub names: Vec<&'static str>, // global symbol table
    pub values: Vec<Type>,        // types of globals
}

pub struct CheckEnv {}

pub fn type_check(alloc: &impl Allocator<'static>, stmts: &[Stmt]) {}
