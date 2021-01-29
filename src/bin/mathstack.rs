use std::{fs, env};
use mathstack::mathstack_parser;
use mathstack::codegen::tree_printer::TreePrinter;
use mathstack::codegen::asm_tree_walker::AsmTreeWalker;
use mathstack::interpreter::asm_interpreter::AsmInterpreter;
use mathstack::ast::ast_node::ASTNodeTrait;
use mathstack::parser::parse_error;
use mathstack::errors::CompilerError;
use mathstack::interpreter::program::Program;
use mathstack::codegen::scope_walker::ScopeWalker;
use mathstack::semantic::scope_collection::ScopeCollection;

const DEFAULT_FILE: &str = "mathfile.c";

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if let Some(name) = args.get(1) {
        name
    } else {
        DEFAULT_FILE
    };

    if let Ok(program) = compile_file(filename) {
        let mut interpreter = AsmInterpreter::default();

        interpreter.load(program);

        // println!("{:?}", asm_walker.instructions);
        interpreter.exec();
    } else {
        panic!("unable to compile {}", filename)
    }
}

fn compile_file(filename: &str) -> Result<Program, CompilerError> {
    let file_content = fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("cannot read file: {}", filename));

    let program = mathstack_parser::ProgramParser::new()
        .parse(&file_content);

    let program = match program {
        Ok(prog) => prog,
        Err(e) => {
            parse_error::handle_parse_error(filename, &file_content, &e);
            return Err(CompilerError::ParseError);
        }
    };

    // println!("{:?}", program);

    let mut walker = TreePrinter::new();
    program.visit(&mut walker);

    let mut scope_walker = ScopeWalker::new(filename);
    program.visit(&mut scope_walker);

    let mut asm_walker = AsmTreeWalker::new(ScopeCollection::from(scope_walker));
    program.visit(&mut asm_walker);
    // print!("{:?}", asm_walker.instructions);
    for s in asm_walker.listing() {
        println!("{}", s);
    }

    Ok(asm_walker.to_program())
}
