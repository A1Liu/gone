use gone::filedb::*;
use std::fs::read_to_string;

pub fn main() {
    let mut files = FileDb::new();
    let file = "test/hello_world.gme";
    files.add(file, &read_to_string(file).unwrap()).unwrap();
    gone::compile(&files).unwrap();
}
