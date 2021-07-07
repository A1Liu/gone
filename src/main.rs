#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use gone::filedb::*;
use gone::util::*;
use std::fs::read_to_string;

pub fn main() {
    let mut files = FileDb::new();
    let file = "test/hello_world.gme";
    files.add(file, &read_to_string(file).unwrap()).unwrap();
    gone::compile(&files).unwrap();
}
