mod mathstack_parser;

use std::{fs, env};
use crate::ast::ast_node::ASTNode;

mod ast;
mod asm;

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
    println!("{:?}", program.to_asm());
}


