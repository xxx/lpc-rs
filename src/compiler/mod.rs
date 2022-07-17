use lpc_rs_errors::{LpcError, Result};

use crate::{
    interpreter::{process::Process, program::Program},
    lpc_parser,
};
use ast::{ast_node::AstNodeTrait, program_node::ProgramNode};
use codegen::{
    codegen_walker::CodegenWalker, default_params_walker::DefaultParamsWalker,
    function_prototype_walker::FunctionPrototypeWalker, inheritance_walker::InheritanceWalker,
    scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
    tree_walker::ContextHolder,
};
use compilation_context::CompilationContext;
use lexer::{Spanned, Token, TokenVecWrapper};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::span::Span;
use lpc_rs_utils::config::Config;
use preprocessor::Preprocessor;
use std::{cell::RefCell, ffi::OsStr, fmt::Debug, io::ErrorKind, rc::Rc};
use tracing::instrument;
use lpc_rs_core::read_lpc_file;

pub mod ast;
pub mod codegen;
pub mod compilation_context;
pub mod lexer;
pub mod parser;
pub mod preprocessor;
pub mod semantic;

#[macro_export]
macro_rules! apply_walker {
    ($walker:ty, $program:expr, $context:expr, $fatal:expr) => {{
        let mut walker = <$walker>::new($context);
        let result = $program.visit(&mut walker);

        let context = walker.into_context();

        if let Err(e) = result {
            let e = e.with_additional_errors(context.errors);
            // e.emit_diagnostics();
            return Err(e);
        } else if $fatal && !context.errors.is_empty() {
            // TODO: get rid of this clone
            let mut e = context.errors[0].clone();

            e = e.with_additional_errors(context.errors[1..].to_vec());
            // e.emit_diagnostics();
            return Err(e);
        }

        context
    }};
}

#[derive(Debug, Default)]
pub struct Compiler {
    /// The configuration to be used for this instance of the compiler
    config: Rc<Config>,

    /// The current depth in the inheritance chain of this compiler
    inherit_depth: usize,

    /// Pointer to the simul_efuns to be used for this compilation
    simul_efuns: Option<Rc<RefCell<Process>>>,
}

impl Compiler {
    /// Create a new `Compiler` with the passed [`Config`]
    #[instrument]
    pub fn new(config: Rc<Config>) -> Self {
        Self {
            config,
            inherit_depth: 0,
            simul_efuns: None,
        }
    }

    /// Set the inherit_depth of a compiler
    pub fn with_inherit_depth(mut self, depth: usize) -> Self {
        self.inherit_depth = depth;
        self
    }

    pub fn with_simul_efuns(mut self, simul_efuns: Option<Rc<RefCell<Process>>>) -> Self {
        self.simul_efuns = simul_efuns;
        self
    }

    /// Fully compile a file into a Program struct
    ///
    /// # Arguments
    /// `path` - The full, on-server path of the file to compile. Also used for error messaging.
    ///          If the file in question ends in `.c`, the extension can be left off, per
    ///          common LPC usage.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::compiler::Compiler;
    ///
    /// let compiler = Compiler::default();
    /// let prog = compiler.compile_file("tests/fixtures/code/example.c").expect("Unable to compile.");
    /// ```
    #[instrument(skip(self))]
    pub fn compile_file<T>(&self, path: T) -> Result<Program>
    where
        T: Into<LpcPath> + Debug,
    {
        let lpc_path = path.into();
        let absolute = lpc_path.as_server(self.config.lib_dir());

        let file_content = match read_lpc_file(&*absolute) {
            Ok(s) => s,
            Err(e) => {
                return match e.kind() {
                    ErrorKind::NotFound => {
                        if matches!(absolute.extension().and_then(OsStr::to_str), Some("c")) {
                            return Err(LpcError::new(format!(
                                "Cannot read file `{}`: {}",
                                absolute.display(),
                                e
                            )));
                        }

                        let dot_c = lpc_path.with_extension("c");
                        self.compile_file(dot_c)
                    }
                    _ => Err(LpcError::new(format!(
                        "Cannot read file `{}`: {}",
                        absolute.display(),
                        e
                    ))),
                };
            }
        };

        self.compile_string(lpc_path, file_content)
    }

    /// Intended for in-game use to be able to compile a file with relative pathname handling
    #[instrument(skip(self))]
    pub fn compile_in_game_file(&self, path: &LpcPath, span: Option<Span>) -> Result<Program> {
        let true_path = path.as_server(self.config.lib_dir());

        if path.as_os_str().is_empty() || !true_path.starts_with(self.config.lib_dir()) {
            return Err(LpcError::new(&format!(
                "attempt to access a file outside of lib_dir: `{}` (expanded to `{}`) (lib_dir: `{}`)",
                path,
                true_path.display(),
                self.config.lib_dir()
            )).with_span(span));
        }

        self.compile_file(path)
    }

    /// Take a str and preprocess it into a vector of Span tuples, and also returns the Preprocessor
    /// used.
    ///
    /// # Arguments
    /// `path` - The absolute on-server path to the file represented by `code`
    /// `code` - The actual code to preprocess.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::compiler::Compiler;
    ///
    /// let code = r#"
    ///     #define COOL_NUMBER 123
    ///     int j = COOL_NUMBER;
    ///
    ///     int square() {
    ///         return j * j;
    ///     }
    /// "#;
    ///
    /// let compiler = Compiler::default();
    /// let (tokens, preprocessor) = compiler.preprocess_string("~/my_file.c", code)
    ///     .expect("Failed to preprocess.");
    /// ```
    #[instrument(skip(self, code))]
    pub fn preprocess_string<T, U>(
        &self,
        path: T,
        code: U,
    ) -> Result<(Vec<Spanned<Token>>, Preprocessor)>
    where
        T: Into<LpcPath> + Debug,
        U: AsRef<str> + Debug,
    {
        let lpc_path = path.into();
        let context = CompilationContext::new(&lpc_path, self.config.clone())
            .with_inherit_depth(self.inherit_depth)
            .with_simul_efuns(self.simul_efuns.clone());

        let mut preprocessor = Preprocessor::new(context);

        preprocessor.scan(&lpc_path, &code).map(|tokens| (tokens, preprocessor))
    }

    /// Compile a string containing an LPC program into a Program struct
    ///
    /// # Arguments
    /// `path` - The absolute on-server path to the file being represented by `code`
    /// `code` - The actual code to be compiled.
    /// # Examples
    /// ```
    /// use lpc_rs::compiler::Compiler;
    ///
    /// let code = r#"
    ///     int j = 123;
    ///
    ///     int square() {
    ///         return j * j;
    ///     }
    /// "#;
    ///
    /// let compiler = Compiler::default();
    /// let prog = compiler.compile_string("~/my_file.c", code).expect("Failed to compile.");
    /// ```
    #[instrument(skip(self, code))]
    pub fn compile_string<T, U>(&self, path: T, code: U) -> Result<Program>
    where
        T: Into<LpcPath> + Debug,
        U: AsRef<str> + Debug,
    {
        let (mut program_node, context) = self.parse_string(&path.into(), code)?;

        // println!("{:?}", program);

        // let mut printer = TreePrinter::new();
        // let _ = program.visit(&mut printer);

        let context = apply_walker!(InheritanceWalker, program_node, context, true);
        let context = apply_walker!(FunctionPrototypeWalker, program_node, context, false);
        let context = apply_walker!(ScopeWalker, program_node, context, false);
        let context = apply_walker!(DefaultParamsWalker, program_node, context, false);
        let context = apply_walker!(SemanticCheckWalker, program_node, context, true);
        let mut asm_walker = CodegenWalker::new(context);

        if let Err(e) = program_node.visit(&mut asm_walker) {
            // e.emit_diagnostics();
            return Err(e);
        }

        // for s in asm_walker.listing() {
        //     println!("{}", s);
        // }

        let program = match asm_walker.into_program() {
            Ok(p) => p,
            Err(e) => return Err(e),
        };

        println!("{}", program.filename);
        for s in program.listing() {
            println!("{}", s);
        }
        println!();

        // let msgpack = program.to_msgpack();
        // println!("{:?}", msgpack.len());
        // println!("{:?}", Program::from(msgpack));
        Ok(program)
    }

    /// Preprocess, then parse a string of code for the file at `path`
    ///
    /// # Returns
    /// A [`Result`] with a tuple containing the parsed [`ProgramNode`],
    /// as well as the [`Preprocessor`]'s [`CompilationContext`]
    #[instrument(skip(self, code))]
    pub fn parse_string<T>(
        &self,
        path: &LpcPath,
        code: T,
    ) -> Result<(ProgramNode, CompilationContext)>
    where
        T: AsRef<str> + Debug,
    {
        let (tokens, preprocessor) = self.preprocess_string(path, code)?;

        let wrapper = TokenVecWrapper::new(&tokens);
        let context = preprocessor.into_context();

        lpc_parser::ProgramParser::new()
            .parse(&context, wrapper)
            .map(|p| (p, context))
            .map_err(|e| LpcError::from(e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod test_compile_file {
        use super::*;

        #[test]
        fn tries_dot_c() {
            let compiler = Compiler::default();

            assert!(compiler.compile_file("tests/fixtures/code/example").is_ok());
        }
    }

    mod test_compile_in_game_file {
        use super::*;

        #[test]
        fn disallows_going_outside_the_root() {
            let config: Rc<Config> = Config::new(None::<&str>)
                .unwrap()
                .with_lib_dir("tests")
                .into();
            let compiler = Compiler::new(config.clone());
            let server_path = LpcPath::new_server("../../secure.c");
            let in_game_path = LpcPath::new_in_game("../../secure.c", "/", config.lib_dir());

            assert!(compiler
                .compile_in_game_file(&server_path, None)
                .unwrap_err()
                .to_string()
                .starts_with("attempt to access a file outside of lib_dir"));

            assert!(compiler
                .compile_in_game_file(&in_game_path, None)
                .unwrap_err()
                .to_string()
                .starts_with("attempt to access a file outside of lib_dir"));
        }
    }
}
