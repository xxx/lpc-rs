use std::{env, fs};
use std::borrow::BorrowMut;

use mathstack::{errors, mathstack_parser};
use mathstack::ast::ast_node::ASTNodeTrait;
use mathstack::codegen::asm_tree_walker::AsmTreeWalker;
use mathstack::codegen::scope_walker::ScopeWalker;
use mathstack::codegen::semantic_check_walker::SemanticCheckWalker;
use mathstack::codegen::tree_printer::TreePrinter;
use mathstack::codegen::tree_walker::TreeWalker;
use mathstack::errors::CompilerError;
use mathstack::interpreter::asm_interpreter::AsmInterpreter;
use mathstack::interpreter::program::Program;
use mathstack::errors::parse_error::ParseError;
use mathstack::semantic::scope_tree::ScopeTree;

const DEFAULT_FILE: &str = "mathfile.c";

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if let Some(name) = args.get(1) {
        name
    } else {
        DEFAULT_FILE
    };

    match compile_file(filename) {
        Ok(program) => {
            let mut interpreter = AsmInterpreter::default();

            interpreter.load(program);

            // println!("{:?}", asm_walker.instructions);
            interpreter.exec();
        },
        Err(e) => {
            panic!("unable to compile {}: {:?}", filename, e)
        }
    }
}

/// Fully compile a file into a Program object
fn compile_file(filename: &str) -> Result<Program, CompilerError> {
    let file_content = fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("cannot read file: {}", filename));

    let mut errors: Vec<CompilerError> = vec![];

    let program = mathstack_parser::ProgramParser::new()
        .parse(&file_content);

    let program = match program {
        Ok(prog) => prog,
        Err(e) => {
            errors.push(CompilerError::ParseError(ParseError::from(e)));
            errors::emit_diagnostics(filename, &file_content, &errors);

            return Err(CompilerError::MultiError(errors));
        }
    };

    // println!("{:?}", program);

    // let mut walker = TreePrinter::new();
    // program.visit(&mut walker);

    let mut scope_walker = ScopeWalker::new(filename);

    program.visit(scope_walker.borrow_mut());

    if scope_walker.get_errors().len() > 0 {
        errors.append(scope_walker.get_errors().to_vec().borrow_mut());
    }

    let scope_tree = ScopeTree::from(scope_walker);

    let mut semantic_check_walker = SemanticCheckWalker::new(&scope_tree);
    program.visit(semantic_check_walker.borrow_mut());

    if semantic_check_walker.get_errors().len() > 0 {
        errors.append(semantic_check_walker.get_errors().to_vec().borrow_mut());
    }

    if errors.len() > 0 {
        errors::emit_diagnostics(filename, &file_content, &errors);
        return Err(CompilerError::MultiError(errors));
    }

    let mut asm_walker = AsmTreeWalker::new(scope_tree);
    program.visit(&mut asm_walker);
    // print!("{:?}", asm_walker.instructions);
    // for s in asm_walker.listing() {
    //     println!("{}", s);
    // }

    Ok(asm_walker.to_program())
}
