#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

mod ast;
mod buckets;
mod filedb;
mod parser;
#[macro_use]
mod util;

#[cfg(test)]
mod test;

use util::*;

fn compile(files: &filedb::FileDb) -> Result<(), Vec<Error>> {
    use codespan_reporting::files::Files;
    use parser::*;
    let ids = files.file_ids();

    let allocator = buckets::BucketList::with_capacity(4096);
    let mut asts = Vec::with_capacity(ids.len());
    let mut errs = Vec::new();
    let symbols = Symbols::new();
    for id in ids {
        let source = files.source(id).unwrap();
        let ast = match lang_grammar::stmt_list(source, &allocator, &symbols) {
            Ok(ast) => ast,
            Err(e) => {
                errs.push((id, e));
                continue;
            }
        };
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
