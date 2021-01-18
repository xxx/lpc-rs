use std::{fs, env};
use mathstack::mathstack_parser;
use mathstack::codegen::tree_printer::TreePrinter;
use mathstack::codegen::asm_tree_walker::AsmTreeWalker;
use mathstack::interpreter::asm_interpreter::AsmInterpreter;
use mathstack::ast::ast_node::ASTNodeTrait;
use mathstack::parser::parse_error;

const DEFAULT_FILE: &str = "mathfile.c";

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if let Some(name) = args.get(1) {
        name
    } else {
        DEFAULT_FILE
    };
    let file_content = fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("cannot read file: {}", filename));

    let program = mathstack_parser::ProgramParser::new()
        .parse(&file_content);

    let program = match program {
        Ok(prog) => prog,
        Err(e) => {
            parse_error::handle_parse_error(filename, &file_content, &e);
            panic!();
        }
    };

    let mut walker = TreePrinter::new();
    program.visit(&mut walker);

    let mut asm_walker: AsmTreeWalker = Default::default();
    program.visit(&mut asm_walker);
    // print!("{:?}", asm_walker.instructions);
    for s in asm_walker.listing() {
        println!("{}", s);
    }

    let mut interpreter: AsmInterpreter = Default::default();

    interpreter.load(&asm_walker.instructions, &asm_walker.combined_labels(), &asm_walker.function_map());

    interpreter.exec();
}


