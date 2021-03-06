use std::fs;

use crate::{
    ast::ast_node::ASTNodeTrait,
    codegen::{
        asm_tree_walker::AsmTreeWalker, default_params_walker::DefaultParamsWalker,
        scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
        tree_walker::TreeWalker,
    },
    errors,
    errors::compiler_error::{parse_error::ParseError, CompilerError},
    interpreter::program::Program,
    lpc_parser,
    preprocessor::Preprocessor,
    semantic::scope_tree::ScopeTree,
};

use crate::{context::Context, parser::lexer::LexWrapper};
use std::path::Path;

/// Fully compile a file into a Program struct
///
/// # Arguments
/// `filename` - The name of the file to compile. Also used for error messaging.
pub fn compile_file<T>(filename: T) -> Result<Program, CompilerError>
where
    T: AsRef<Path> + AsRef<str>
{
    let file_content =
        fs::read_to_string(&filename).unwrap_or_else(|_| panic!("cannot read file: {}", AsRef::<Path>::as_ref(&filename).display()));

    compile_string(filename, file_content)
}

/// Compile a string containing an LPC program into a Program struct
///
/// # Arguments
/// `filename` - The name of the file being compiled. Used for error messaging.
/// `code` - The actual code to be compiled.
pub fn compile_string<T, U>(filename: T, code: U) -> Result<Program, CompilerError>
where
    T: AsRef<Path> + AsRef<str>,
    U: AsRef<str>,
{
    let mut errors: Vec<CompilerError> = vec![];

    let context = Context::new(&filename, ".", Vec::new());
    let mut preprocessor = Preprocessor::new(context);
    let code = match preprocessor.scan(&filename, ".", &code) {
        Ok(c) => c,
        Err(e) => {
            errors.push(CompilerError::PreprocessorError(e));
            errors::emit_diagnostics(&*AsRef::<Path>::as_ref(&filename).to_string_lossy(), &errors);

            return Err(CompilerError::MultiError(errors));
        }
    };

    let mut context = preprocessor.into_context();

    let program = lpc_parser::ProgramParser::new().parse(LexWrapper::new(&code));

    let mut program = match program {
        Ok(prog) => prog,
        Err(e) => {
            errors.push(CompilerError::ParseError(ParseError::from(e)));
            errors::emit_diagnostics(&*AsRef::<Path>::as_ref(&filename).to_string_lossy(), &errors);

            // Parse errors are fatal, so we're done here.
            return Err(CompilerError::MultiError(errors));
        }
    };

    // println!("{:?}", program);

    // let mut printer = TreePrinter::new();
    // let _ = program.visit(&mut printer);

    let mut scope_walker = ScopeWalker::new(context);

    let _ = program.visit(&mut scope_walker);

    let context = scope_walker.into_context();

    let mut default_params_walker = DefaultParamsWalker::new(context);
    let _ = program.visit(&mut default_params_walker);

    let context = default_params_walker.into_context();

    let mut semantic_check_walker = SemanticCheckWalker::new(context);
    let _ = program.visit(&mut semantic_check_walker);
    // if !context.errors.is_empty() {
    //     errors.append(&mut context.errors.to_vec());
    // }

    let context = semantic_check_walker.into_context();

    if !context.errors.is_empty() {
        errors::emit_diagnostics(&AsRef::<Path>::as_ref(&filename).to_string_lossy(), &context.errors);
        return Err(CompilerError::MultiError(context.errors));
    }

    // let scope_tree = ScopeTree::from(context);
    let mut asm_walker = AsmTreeWalker::new(context);
    let _ = program.visit(&mut asm_walker);
    // print!("{:?}", asm_walker.instructions);
    for s in asm_walker.listing() {
        println!("{}", s);
    }

    let program = asm_walker.to_program(&AsRef::<Path>::as_ref(&filename).to_string_lossy());

    let msgpack = program.to_msgpack();
    println!("{:?}", msgpack.len());
    println!("{:?}", Program::from(msgpack));
    Ok(program)
}
