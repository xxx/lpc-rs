use std::{fs, env};
use mathstack::mathstack_parser;
use mathstack::codegen::tree_walker::TreeWalker;
use mathstack::codegen::tree_printer::TreePrinter;
use mathstack::codegen::asm_tree_walker::AsmTreeWalker;
// use mathstack::interpreter::asm_interpreter::AsmInterpreter;

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

    let program = mathstack_parser::ProgramParser::new()
        .parse(&file_content)
        .expect("unsuccessful parse");

    let mut walker = TreePrinter::new();

    walker.walk_tree(&program);

    let mut asm_walker: AsmTreeWalker = Default::default();
    asm_walker.walk_tree(&program);
    // print!("{:?}", asm_walker.instructions);
    print!("{:?}", asm_walker.listing());

    // let mut interpreter: AsmInterpreter = Default::default();
    // interpreter.load(&asm_walker.instructions);
    //
    // interpreter.eval();
}


