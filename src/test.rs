use crate::compile;
use crate::filedb::*;
use crate::util::*;
use std::fs::{read_dir, read_to_string};

fn test_file_should_succeed(files: &FileDb, output_file: Option<&str>) {
    compile(files).unwrap();
}

// fn test_file_compile_should_fail(filename: &str) {
//     let config = codespan_reporting::term::Config::default();
//     let mut files = FileDb::new(true);
//     let mut writer = StringWriter::new();
//
//     files.add_from_fs(filename).unwrap();
//
//     match compile(&mut files) {
//         Err(errs) => {
//             emit_err(&errs, &files, &mut writer);
//             println!("{}", writer.to_string());
//         }
//         _ => panic!("should have failed"),
//     }
// }

// fn test_file_runtime_should_fail(filename: &str, expected_err: &str) {
//     let config = codespan_reporting::term::Config::default();
//     let mut files = FileDb::new(true);
//     let mut writer = StringWriter::new();
//
//     files.add_from_fs(filename).unwrap();
//
//     let program = match compile(&mut files) {
//         Ok(program) => program,
//         Err(errs) => {
//             emit_err(&errs, &files, &mut writer);
//             println!("{}", writer.into_string());
//             panic!();
//         }
//     };
//     mem::drop(files);
//
//     let mut runtime = Runtime::new(program, StringArray::new());
//     let diag = runtime.run(&mut writer);
//     for (idx, op) in runtime.program.ops.iter().enumerate() {
//         println!("op {}: {:?}", idx, op);
//     }
//
//     let err = match diag.status {
//         RuntimeStatus::ErrorExited(err) => err,
//         x => panic!("{:?}", x),
//     };
//
//     assert_eq!(err.short_name, expected_err);
// }

macro_rules! gen_test_should_succeed {
    ( $( $ident:ident ),* ) => {
        $(
            gen_test_should_succeed!(@S, $ident);
        )*
    };
    (@S, $ident:ident) => {
            #[test]
            fn $ident() {
                use std::path::Path;

                let file_path = concat!("test/", stringify!($ident), ".gme");
                let folder_path = concat!("test/", stringify!($ident));

                let mut files = FileDb::new();
                if Path::new(file_path).exists() {
                    let out_path = concat!("lib/test/", stringify!($ident), ".gme.out");
                    files.add(file_path, &read_to_string(file_path).unwrap()).unwrap();
                    test_file_should_succeed(&files, Some(out_path));
                    return;
                }

                let mut out_path = None;
                for entry in read_dir(Path::new(folder_path)).unwrap() {
                    let path = entry.unwrap().path();
                    let file_path = path.to_str().unwrap();
                    if file_path.ends_with(".out") {
                        out_path = Some(path);
                        continue;
                    }

                    files.add(file_path, &read_to_string(file_path).unwrap()).unwrap();
                }

                let out_path_ref = out_path.as_ref().map(|a| a.to_str()).flatten();
                test_file_should_succeed(&files, out_path_ref);
            }
    };

}

// macro_rules! gen_test_runtime_should_fail {
//     ( $( ($ident:ident, $expr:expr ) ),* ) => {
//         $(
//             #[test]
//             fn $ident() {
//                 test_file_runtime_should_fail(concat!("test/", stringify!($ident), ".c"), $expr);
//             }
//         )*
//     };
// }

gen_test_should_succeed!(hello_world);

// gen_test_runtime_should_fail!((stack_locals, "InvalidPointer"));
//
//

// use std::sync::atomic::*;
// static SHOULD_FAIL: AtomicBool = AtomicBool::new(false);
