use std::fs;

use crate::{
    ast::ast_node::ASTNodeTrait,
    codegen::{
        asm_tree_walker::AsmTreeWalker, default_params_walker::DefaultParamsWalker,
        scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
    },
    errors,
    errors::compiler_error::{parse_error::ParseError, CompilerError},
    interpreter::program::Program,
    lpc_parser,
    preprocessor::Preprocessor,
};

use crate::{codegen::tree_walker::ContextHolder, context::Context};
use std::path::Path;
use crate::parser::lexer::{TokenVecWrapper, Spanned, Token};

/// Fully compile a file into a Program struct
///
/// # Arguments
/// `filename` - The name of the file to compile. Also used for error messaging.
pub fn compile_file<T>(filename: T) -> Result<Program, CompilerError>
where
    T: AsRef<Path> + AsRef<str>,
{
    let file_content = fs::read_to_string(&filename).unwrap_or_else(|_| {
        panic!(
            "cannot read file: {}",
            AsRef::<Path>::as_ref(&filename).display()
        )
    });

    compile_string(filename, file_content)
}

/// Take a string and preprocess it into a vector of Span tuples
pub fn preprocess_string<T, U>(filename: T, code: U) -> Result<(Vec<Spanned<Token>>, Preprocessor), CompilerError>
    where
        T: AsRef<Path> + AsRef<str>,
        U: AsRef<str>,
{
    let context = Context::new(&filename, ".", Vec::new());

    let mut preprocessor = Preprocessor::new(context);
    let code = match preprocessor.scan(&filename, ".", &code) {
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
/// `filename` - The name of the file being compiled. Used for error messaging.
/// `code` - The actual code to be compiled.
pub fn compile_string<T, U>(filename: T, code: U) -> Result<Program, CompilerError>
where
    T: AsRef<Path> + AsRef<str>,
    U: AsRef<str>,
{
    let (code, preprocessor) = preprocess_string(&filename, code)?;

    let code = TokenVecWrapper::new(code);
    let context = preprocessor.into_context();

    let program = lpc_parser::ProgramParser::new().parse(code);

    let mut program = match program {
        Ok(prog) => prog,
        Err(e) => {
            let err = CompilerError::ParseError(ParseError::from(e));
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

    let program = asm_walker.to_program(&AsRef::<Path>::as_ref(&filename).to_string_lossy());

    let msgpack = program.to_msgpack();
    println!("{:?}", msgpack.len());
    println!("{:?}", Program::from(msgpack));
    Ok(program)
}
