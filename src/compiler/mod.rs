use std::path::Path;

use compiler_error::CompilerError;
use fs_err as fs;

use crate::{
    ast::ast_node::AstNodeTrait,
    codegen::{
        asm_tree_walker::AsmTreeWalker, default_params_walker::DefaultParamsWalker,
        scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
        tree_walker::ContextHolder,
    },
    context::Context,
    errors,
    errors::LpcError,
    interpreter::program::Program,
    lpc_parser,
    parser::{
        lexer::{Spanned, Token, TokenVecWrapper},
        span::Span,
    },
    preprocessor::Preprocessor,
    util::config::Config,
};
use std::{fmt::Debug, io::ErrorKind, rc::Rc};
use crate::util::path_maker::LpcPath;

pub mod compiler_error;

#[derive(Debug, Default)]
pub struct Compiler {
    config: Rc<Config>,
}

impl Compiler
// where
//     LpcPath: From<&P>
{
    /// Create a new `Compiler` with the passed [`Config`]
    pub fn new(config: Rc<Config>) -> Self {
        Self { config }
    }

    /// Fully compile a file into a Program struct
    ///
    /// # Arguments
    /// `path` - The full, on-server path of the file to compile. Also used for error messaging.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::compiler::Compiler;
    ///
    /// let compiler = Compiler::default();
    /// let prog = compiler.compile_file("tests/fixtures/code/example.c").expect("Unable to compile.");
    /// ```
    pub fn compile_file<T>(&self, path: T) -> Result<Program, CompilerError>
    where
        T: AsRef<Path> + Into<LpcPath>,
    {
        let lpc_path = path.into();
        let absolute = lpc_path.as_server(self.config.lib_dir());
        let file_content = match fs::read_to_string(&*absolute) {
            Ok(s) => s,
            Err(e) => {
                return match e.kind() {
                    ErrorKind::NotFound => {
                        // let mut dot_c = AsRef::<str>::as_ref(&path).to_string();

                        if absolute.ends_with(".c") {
                            return Err(CompilerError::LpcError(LpcError::new(format!(
                                "Cannot read file `{}`: {}",
                                absolute.display(),
                                e
                            ))));
                        }

                        let mut owned = absolute.into_owned().into_os_string();
                        owned.push(".c");
                        self.compile_file(owned)
                    }
                    _ => Err(CompilerError::LpcError(LpcError::new(format!(
                        "Cannot read file `{}`: {}",
                        absolute.display(),
                        e
                    )))),
                }
            }
        };

        self.compile_string(absolute, file_content)
    }

    /// Intended for in-game use to be able to compile a file with relative pathname handling
    pub fn compile_in_game_file<T>(
        &self,
        path: T,
        // cwd: U,
        span: Option<Span>,
    ) -> Result<Program, CompilerError>
    where
        T: AsRef<Path> + Into<LpcPath>,
        // U: AsRef<Path>,
    {
        let lpc_path = path.into();
        let absolute_file_path = lpc_path.as_server(self.config.lib_dir());

        if !absolute_file_path.starts_with(self.config.lib_dir()) {
            return Err(CompilerError::LpcError(LpcError::new(&format!(
                "Attempt to access a file outside of lib_dir: `{}` (expanded to `{}`) (lib_dir: `{}`)",
                AsRef::<Path>::as_ref(&*absolute_file_path).display(),
                absolute_file_path.display(),
                self.config.lib_dir()
            )).with_span(span)));
        }

        self.compile_file(absolute_file_path)
    }

    /// Take a str and preprocess it into a vector of Span tuples, and also returns the Preprocessor
    /// used.
    ///
    /// # Arguments
    /// `path` - The in-game path to the file being preprocessed. Used for resolving
    ///   relative `#include` paths. It's assumed the CWD is already prepended.
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
    pub fn preprocess_string<T, U>(
        &self,
        path: T,
        code: U,
    ) -> Result<(Vec<Spanned<Token>>, Preprocessor), CompilerError>
    where
        T: AsRef<Path> + Into<LpcPath>,
        U: AsRef<str>,
    {
        let context = Context::new(&path, self.config.clone());

        let mut preprocessor = Preprocessor::new(context);
        let code = match preprocessor.scan(&path, &code) {
            Ok(c) => c,
            Err(e) => {
                let err = e;

                errors::emit_diagnostics(&[err.clone()]);

                // Preprocessor errors are fatal.
                return Err(CompilerError::LpcError(err));
            }
        };

        Ok((code, preprocessor))
    }

    /// Compile a string containing an LPC program into a Program struct
    ///
    /// # Arguments
    /// `path` - The in-game path of the file being compiled. Used for error messaging.
    ///   It's assumed the CWD is already prepended.
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

    pub fn compile_string<T, U>(&self, path: T, code: U) -> Result<Program, CompilerError>
    where
        T: AsRef<Path> + Into<LpcPath>,
        U: AsRef<str>,
    {
        let (code, preprocessor) = self.preprocess_string(&path, code)?;

        let code = TokenVecWrapper::new(&code);
        let context = preprocessor.into_context();

        let program = lpc_parser::ProgramParser::new().parse(&context, code);

        let mut program = match program {
            Ok(prog) => prog,
            Err(e) => {
                let err = LpcError::from(e);
                errors::emit_diagnostics(&[err.clone()]);

                // Parse errors are fatal, so we're done here.
                return Err(CompilerError::LpcError(err));
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
            return Err(CompilerError::Collection(context.errors));
        }

        // let scope_tree = ScopeTree::from(context);
        let mut asm_walker = AsmTreeWalker::new(context);

        if let Err(e) = program.visit(&mut asm_walker) {
            errors::emit_diagnostics(&[e.clone()]);
            return Err(CompilerError::LpcError(e));
        }

        // for s in asm_walker.listing() {
        //     println!("{}", s);
        // }

        let program = match asm_walker.to_program() {
            Ok(p) => p,
            Err(e) => return Err(CompilerError::LpcError(e)),
        };

        // let msgpack = program.to_msgpack();
        // println!("{:?}", msgpack.len());
        // println!("{:?}", Program::from(msgpack));
        Ok(program)
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
            let config = Config::new(None::<&str>).unwrap().with_lib_dir("tests");
            let compiler = Compiler::new(config.into());

            assert!(compiler
                .compile_in_game_file("../../secure.c", None)
                .unwrap_err()
                .to_string()
                .starts_with("Attempt to access a file outside of lib_dir"));
        }
    }
}
