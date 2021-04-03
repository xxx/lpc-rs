use std::fs;

use crate::{
    ast::ast_node::AstNodeTrait,
    codegen::{
        asm_tree_walker::AsmTreeWalker, default_params_walker::DefaultParamsWalker,
        scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
    },
    errors,
    errors::compiler_error::CompilerError,
    interpreter::program::Program,
    lpc_parser,
    preprocessor::Preprocessor,
};
use crate::errors::NewError;

use crate::{
    codegen::tree_walker::ContextHolder,
    context::Context,
    parser::lexer::{Spanned, Token, TokenVecWrapper},
};
use std::path::Path;

/// Fully compile a file into a Program struct
///
/// # Arguments
/// `path` - The path of the file to compile. Also used for error messaging.
///
/// # Examples
/// ```
/// use lpc_rs::compiler::compile_file;
///
/// let prog = compile_file("tests/fixtures/code/example.c").expect("Unable to compile.");
/// ```
pub fn compile_file<T>(path: T) -> Result<Program, CompilerError>
where
    T: AsRef<Path> + AsRef<str>,
{
    let file_content = fs::read_to_string(&path).unwrap_or_else(|_| {
        panic!(
            "cannot read file: {}",
            AsRef::<Path>::as_ref(&path).display()
        )
    });

    compile_string(path, file_content)
}

/// Take a str and preprocess it into a vector of Span tuples
///
/// # Arguments
/// `path` - The path to the file being preprocessed. Used for resolving relative `#include` paths.
/// `code` - The actual code to preprocess.
///
/// # Examples
/// ```
/// use lpc_rs::compiler::preprocess_string;
///
/// let code = r#"
///     int j = 123;
///
///     int square() {
///         return j * j;
///     }
/// "#;
///
/// let (tokens, preprocessor) = preprocess_string("~/my_file.c", code)
///     .expect("Failed to preprocess.");
/// ```
pub fn preprocess_string<T, U>(
    path: T,
    code: U,
) -> Result<(Vec<Spanned<Token>>, Preprocessor), CompilerError>
where
    T: AsRef<Path> + AsRef<str>,
    U: AsRef<str>,
{
    let context = Context::new(&path, ".", Vec::new());

    let cwd = AsRef::<Path>::as_ref(&path).parent().unwrap_or(Path::new("/"));

    let mut preprocessor = Preprocessor::new(context);
    let code = match preprocessor.scan(&path, cwd, &code) {
        Ok(c) => c,
        Err(e) => {
            let err = CompilerError::PreprocessorError(e);

            errors::emit_diagnostics(&[err.clone()]);

            // Preprocessor errors are fatal.
            return Err(err);
        }
    };

    Ok((code, preprocessor))
}

/// Compile a string containing an LPC program into a Program struct
///
/// # Arguments
/// `path` - The path of the file being compiled. Used for error messaging.
/// `code` - The actual code to be compiled.
/// # Examples
/// ```
/// use lpc_rs::compiler::compile_string;
///
/// let code = r#"
///     int j = 123;
///
///     int square() {
///         return j * j;
///     }
/// "#;
///
/// let prog = compile_string("~/my_file.c", code).expect("Failed to compile.");
/// ```

pub fn compile_string<T, U>(path: T, code: U) -> Result<Program, CompilerError>
where
    T: AsRef<Path> + AsRef<str>,
    U: AsRef<str>,
{
    let (code, preprocessor) = preprocess_string(&path, code)?;

    let code = TokenVecWrapper::new(code);
    let context = preprocessor.into_context();

    let program = lpc_parser::ProgramParser::new().parse(code);

    let mut program = match program {
        Ok(prog) => prog,
        Err(e) => {
            let err = CompilerError::NewError(NewError::from(e));
            errors::emit_diagnostics(&[err.clone()]);

            // Parse errors are fatal, so we're done here.
            return Err(err);
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

    let context = semantic_check_walker.into_context();

    if !context.errors.is_empty() {
        errors::emit_diagnostics(&context.errors);
        return Err(CompilerError::MultiError(context.errors));
    }

    // let scope_tree = ScopeTree::from(context);
    let mut asm_walker = AsmTreeWalker::new(context);
    let _ = program.visit(&mut asm_walker);
    // print!("{:?}", asm_walker.instructions);
    for s in asm_walker.listing() {
        println!("{}", s);
    }

    let program = asm_walker.to_program();

    let msgpack = program.to_msgpack();
    println!("{:?}", msgpack.len());
    println!("{:?}", Program::from(msgpack));
    Ok(program)
}
