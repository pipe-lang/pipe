use std::{env, fs};
use pipe::*;

pub fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument")).expect("Failed to read file");

    if let Ok(_) = parse(&src) {
        println!("All good!");
    }
}
