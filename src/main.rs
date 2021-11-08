use std::{env, fs};
use std::path::Path;
use pipe::*;

pub fn main() {
    let file_name = env::args().nth(1).expect("Expected file argument");

    run(&file_name)
}
