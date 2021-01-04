mod mathstack_parser;

use std::{fs, env};
use crate::codegen::tree_walker::TreeWalkerTrait;
use crate::codegen::tree_printer::TreePrinter;

mod ast;
mod asm;
mod codegen;

const DEFAULT_FILE: &str = "mathfile";

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = if let Some(filename) = args.get(1) {
        filename
    } else {
        DEFAULT_FILE
    };
    let file_content = fs::read_to_string(file)
        .unwrap_or_else(|_| panic!("cannot read file: {}", file));
    let program = mathstack_parser::parse_program(&file_content)
        .expect("unsuccessful parse"); // unwrap the parse result

    println!("{:?}", program);

    let mut walker = TreePrinter::new();

    walker.walk_tree(&program);
}


