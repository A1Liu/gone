#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

pub mod ast;
pub mod buckets;
pub mod filedb;
pub mod parser;
pub mod type_checking;
#[macro_use]
pub mod util;

#[cfg(test)]
pub mod test;

use util::*;

const KB: usize = 1024;

pub fn compile(files: &filedb::FileDb) -> Result<(), Vec<Error>> {
    use codespan_reporting::files::Files;
    use parser::*;
    let ids = files.file_ids();

    let allocator = buckets::BucketList::with_capacity(KB * KB * 4);
    let mut asts = Vec::with_capacity(ids.len());
    let mut errs = Vec::new();
    let symbols = Symbols::new(&*allocator);
    for id in ids {
        let source = files.source(id).unwrap();
        let ast = match lang_grammar::stmt_list(source, &*allocator, &symbols) {
            Ok(ast) => ast,
            Err(e) => {
                errs.push((id, e));
                continue;
            }
        };
        eprintln!("{:#?}", ast);
        asts.push(ast);
    }

    let mut errs_out = Vec::with_capacity(errs.len());
    if errs.len() != 0 {
        for (file, err) in errs {
            let err = Error {
                info: "parse error",
                message: Some(err.expected.to_string()),
                begin: err.location.offset,
                end: err.location.offset,
                file,
                #[cfg(debug_assertions)]
                compiler_loc: here!(), // TODO this is dumb
            };

            println!("{}", err.render(files));
            errs_out.push(err);
        }

        return Err(errs_out);
    }

    return Ok(());
}
