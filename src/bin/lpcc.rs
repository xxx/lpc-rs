use std::{env, fs};

use lpc_rs::{
    ast::ast_node::ASTNodeTrait,
    codegen::{
        asm_tree_walker::AsmTreeWalker, default_params_walker::DefaultParamsWalker,
        scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
        tree_printer::TreePrinter, tree_walker::TreeWalker,
    },
    compiler::compile_file,
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
