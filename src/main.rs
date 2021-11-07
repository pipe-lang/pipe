use std::{env, fs};
use pipe::*;

pub fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument")).expect("Failed to read file");

    if let Ok(ast) = parse(&src) {
        println!("{:?}", ast);
    }
}
