mod mathstack_parser;

use std::{fs, env};

mod ast;

const DEFAULT_FILE: &str = "mathfile";

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = if let Some(filename) = args.get(1) {
        filename
    } else {
        DEFAULT_FILE
    };
    let file_content = fs::read_to_string(file)
        .expect(&format!("cannot read file: {}", file));
    let program = mathstack_parser::parse_program(&file_content)
        .expect("unsuccessful parse"); // unwrap the parse result

    println!("{:?}", program);
}


