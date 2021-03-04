use std::fs;

use crate::{
    ast::ast_node::ASTNodeTrait,
    codegen::{
        asm_tree_walker::AsmTreeWalker, default_params_walker::DefaultParamsWalker,
        scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
        tree_printer::TreePrinter, tree_walker::TreeWalker,
    },
    errors,
    errors::compiler_error::{parse_error::ParseError, CompilerError},
    interpreter::program::Program,
    lpc_parser,
    semantic::scope_tree::ScopeTree,
};
use crate::preprocessor::Preprocessor;
use crate::errors::compiler_error::CompilerError::PreprocessorError;

/// Fully compile a file into a Program struct
///
/// # Arguments
/// `filename` - The name of the file to compile. Also used for error messaging.
pub fn compile_file(filename: &str) -> Result<Program, CompilerError> {
    let file_content =
        fs::read_to_string(filename).unwrap_or_else(|_| panic!("cannot read file: {}", filename));

    compile_string(filename, file_content)
}

/// Compile a string containing an LPC program into a Program struct
///
/// # Arguments
/// `filename` - The name of the file being compiled. Used for error messaging.
/// `code` - The actual code to be compiled.
pub fn compile_string(filename: &str, code: String) -> Result<Program, CompilerError> {
    let mut errors: Vec<CompilerError> = vec![];

    let mut preprocessor = Preprocessor::new(".", Vec::new());
    let code = match preprocessor.scan(filename, ".", &code) {
        Ok(c) => c,
        Err(e) => {
            errors.push(CompilerError::PreprocessorError(e));
            errors::emit_diagnostics(filename, &errors);

            return Err(CompilerError::MultiError(errors));
        }
    };

    let program = lpc_parser::ProgramParser::new().parse(&code);

    let mut program = match program {
        Ok(prog) => prog,
        Err(e) => {
            errors.push(CompilerError::ParseError(ParseError::from(e)));
            errors::emit_diagnostics(filename, &errors);

            return Err(CompilerError::MultiError(errors));
        }
    };

    // println!("{:?}", program);

    // let mut printer = TreePrinter::new();
    // let _ = program.visit(&mut printer);

    let mut scope_walker = ScopeWalker::new(filename);

    let _ = program.visit(&mut scope_walker);

    if !scope_walker.get_errors().is_empty() {
        errors.append(&mut scope_walker.get_errors().to_vec());
    }

    let mut default_params_walker = DefaultParamsWalker::new();
    let _ = program.visit(&mut default_params_walker);
    if !default_params_walker.get_errors().is_empty() {
        errors.append(&mut default_params_walker.get_errors().to_vec());
    }

    let mut semantic_check_walker =
        SemanticCheckWalker::new(&scope_walker.scopes, &scope_walker.function_prototypes);
    let _ = program.visit(&mut semantic_check_walker);
    if !semantic_check_walker.get_errors().is_empty() {
        errors.append(&mut semantic_check_walker.get_errors().to_vec());
    }

    if !errors.is_empty() {
        errors::emit_diagnostics(filename, &errors);
        return Err(CompilerError::MultiError(errors));
    }

    let scope_tree = ScopeTree::from(scope_walker);
    let mut asm_walker = AsmTreeWalker::new(scope_tree, default_params_walker.into_functions());
    let _ = program.visit(&mut asm_walker);
    // print!("{:?}", asm_walker.instructions);
    for s in asm_walker.listing() {
        println!("{}", s);
    }

    let program = asm_walker.to_program(filename);

    let msgpack = program.to_msgpack();
    println!("{:?}", msgpack.len());
    println!("{:?}", Program::from(msgpack));
    Ok(program)
}
