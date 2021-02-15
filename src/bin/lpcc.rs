use std::{borrow::BorrowMut, env, fs};

use lpc_rs::{
    ast::ast_node::ASTNodeTrait,
    codegen::{
        asm_tree_walker::AsmTreeWalker, default_params_walker::DefaultParamsWalker,
        scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
        tree_printer::TreePrinter, tree_walker::TreeWalker,
    },
    errors,
    errors::compiler_error::{parse_error::ParseError, CompilerError},
    interpreter::{asm_interpreter::AsmInterpreter, program::Program},
    lpc_parser,
    semantic::scope_tree::ScopeTree,
};

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

            // println!("{:?}", program);
            interpreter.load(program);

            if let Err(e) = interpreter.exec() {
                let file_content = fs::read_to_string(filename)
                    .unwrap_or_else(|_| panic!("cannot read file: {}", filename));
                errors::emit_diagnostics(filename, &file_content, &[e]);
            }
        }
        Err(e) => panic!("unable to compile {}: {:?}", filename, e),
    }
}

/// Fully compile a file into a Program object
fn compile_file(filename: &str) -> Result<Program, CompilerError> {
    let file_content =
        fs::read_to_string(filename).unwrap_or_else(|_| panic!("cannot read file: {}", filename));

    let mut errors: Vec<CompilerError> = vec![];

    let program = lpc_parser::ProgramParser::new().parse(&file_content);

    let mut program = match program {
        Ok(prog) => prog,
        Err(e) => {
            errors.push(CompilerError::ParseError(ParseError::from(e)));
            errors::emit_diagnostics(filename, &file_content, &errors);

            return Err(CompilerError::MultiError(errors));
        }
    };

    // println!("{:?}", program);

    let mut walker = TreePrinter::new();
    let _ = program.visit(&mut walker);

    let mut scope_walker = ScopeWalker::new(filename);

    let _ = program.visit(scope_walker.borrow_mut());

    if !scope_walker.get_errors().is_empty() {
        errors.append(scope_walker.get_errors().to_vec().borrow_mut());
    }

    let mut default_params_walker = DefaultParamsWalker::new();
    let _ = program.visit(default_params_walker.borrow_mut());
    if !default_params_walker.get_errors().is_empty() {
        errors.append(default_params_walker.get_errors().to_vec().borrow_mut());
    }

    let mut semantic_check_walker =
        SemanticCheckWalker::new(&scope_walker.scopes, &scope_walker.function_prototypes);
    let _ = program.visit(semantic_check_walker.borrow_mut());
    if !semantic_check_walker.get_errors().is_empty() {
        errors.append(semantic_check_walker.get_errors().to_vec().borrow_mut());
    }

    if !errors.is_empty() {
        errors::emit_diagnostics(filename, &file_content, &errors);
        return Err(CompilerError::MultiError(errors));
    }

    let scope_tree = ScopeTree::from(scope_walker);
    let mut asm_walker = AsmTreeWalker::new(scope_tree, default_params_walker.into_functions());
    let _ = program.visit(&mut asm_walker);
    // print!("{:?}", asm_walker.instructions);
    for s in asm_walker.listing() {
        println!("{}", s);
    }

    Ok(asm_walker.to_program(filename))
}
