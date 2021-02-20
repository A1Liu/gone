#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

extern crate alloc;

#[macro_use]
mod util;

mod ast;
mod buckets;
mod filedb;
mod lexer;
mod parser;
mod passes;

#[cfg(test)]
mod test;

use filedb::FileDb;
use util::Error;

fn compile(files: &FileDb) -> Result<(), Vec<Error>> {
    let mut lexer = lexer::Lexer::new();
    let ids = files.file_ids();

    let mut asts = Vec::with_capacity(ids.len());
    let mut errs = Vec::with_capacity(ids.len());
    for id in ids {
        let source = files.source(id).unwrap();
        let ast = match parser::parse_file(&mut lexer, id, source) {
            Ok(ast) => ast,
            Err(e) => {
                errs.push(e);
                continue;
            }
        };
        asts.push(ast);
    }

    if errs.len() != 0 {
        for err in &errs {
            println!("{}", err.render(files));
        }

        return Err(errs);
    }

    return Ok(());
}

fn main() {
    use core::mem::{align_of, size_of};

    debug!(size_of::<ast::Expr>());
    debug!(size_of::<ast::Stmt>());
    debug!(size_of::<ast::StmtKind>());
    debug!(align_of::<ast::StmtKind>());
    debug!(size_of::<ast::Type>());
    debug!(size_of::<ast::Decl>());
}
