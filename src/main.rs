use std::{fs, env};
use crate::codegen::tree_walker::TreeWalker;
use crate::codegen::tree_printer::TreePrinter;
use crate::codegen::asm_tree_walker::AsmTreeWalker;

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(#[allow(clippy::all)] pub mathstack); // synthesized by LALRPOP

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

    let program = mathstack::ProgramParser::new()
        .parse(&file_content)
        .expect("unsuccessful parse");

    let mut walker = TreePrinter::new();

    walker.walk_tree(&program);

    let mut asm_walker = AsmTreeWalker::new();
    asm_walker.walk_tree(&program);
    print!("{:?}", asm_walker.instructions);
}


