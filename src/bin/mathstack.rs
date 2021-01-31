use std::{fs, env};
use std::borrow::BorrowMut;
use mathstack::mathstack_parser;
use mathstack::codegen::tree_printer::TreePrinter;
use mathstack::codegen::asm_tree_walker::AsmTreeWalker;
use mathstack::interpreter::asm_interpreter::AsmInterpreter;
use mathstack::ast::ast_node::ASTNodeTrait;
use mathstack::parser::parse_error;
use mathstack::errors::CompilerError;
use mathstack::interpreter::program::Program;
use mathstack::codegen::scope_walker::ScopeWalker;
use mathstack::codegen::semantic_check_walker::SemanticCheckWalker;
use mathstack::semantic::scope_tree::ScopeTree;
use codespan_reporting::term::termcolor::{StandardStream, ColorChoice};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use mathstack::codegen::tree_walker::TreeWalker;
use codespan_reporting::diagnostic::Diagnostic;

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
            parse_error::handle_parse_error(filename, &file_content, &e);
            // parse errors are fatal.
            return Err(CompilerError::ParseError);
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
        emit_diagnostics(filename, &file_content, &errors);
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

fn emit_diagnostics(filename: &str, file_content: &str, errors: &Vec<CompilerError>) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, file_content);

    let diagnostics: Vec<Diagnostic<usize>> = errors
        .iter()
        .flat_map(|e| e.to_diagnostics(file_id))
        .collect();
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    for diagnostic in &diagnostics {
        if let Err(e) = term::emit(&mut writer.lock(), &config, &files, diagnostic) {
            eprintln!("error attempting to emit semantic error: {:?}", e);
        };
    }
}
