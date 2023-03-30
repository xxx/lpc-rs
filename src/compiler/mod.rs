use std::{collections::VecDeque, ffi::OsStr, fmt::Debug, io::ErrorKind, rc::Rc, sync::Arc};

use ast::{ast_node::AstNodeTrait, program_node::ProgramNode};
use codegen::{
    codegen_walker::CodegenWalker, default_params_walker::DefaultParamsWalker,
    function_prototype_walker::FunctionPrototypeWalker, inheritance_walker::InheritanceWalker,
    scope_walker::ScopeWalker, semantic_check_walker::SemanticCheckWalker,
    tree_walker::ContextHolder,
};
use compilation_context::CompilationContext;
use derive_builder::Builder;
use educe::Educe;
use lexer::{Spanned, Token, TokenVecWrapper};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{span::Span, LpcError, LpcErrorSeverity, Result};
use lpc_rs_utils::{config::Config, read_lpc_file};
use preprocessor::Preprocessor;
use qcell::{QCell, QCellOwner};
use tracing::instrument;

use crate::{
    compiler::{ast::inherit_node::InheritNode, compilation_context::CompilationContextBuilder},
    interpreter::{process::Process, program::Program},
    lpc_parser,
    util::qcell_process_option_debug,
};

pub mod ast;
pub mod codegen;
pub mod compilation_context;
pub mod lexer;
pub mod parser;
pub mod preprocessor;
pub mod semantic;

#[macro_export]
macro_rules! apply_walker {
    ($walker:ty, $program:expr, $context:expr, $cell_key:expr, $fatal:expr) => {{
        let mut walker = <$walker>::new($context);
        let result = $program.visit(&mut walker, $cell_key);

        let mut context = walker.into_context();

        if let Err(e) = result {
            let e = e.with_additional_errors(context.errors);
            return Err(e);
        } else if $fatal
            && context
                .errors
                .iter()
                .any(|e| e.severity == LpcErrorSeverity::Error)
        {
            let mut errors = std::mem::take(&mut context.errors);
            // put all warnings first, but otherwise keep them in the original order
            errors.sort_by(|a, b| match (a.severity, b.severity) {
                (LpcErrorSeverity::Warning, _) => std::cmp::Ordering::Less,
                (_, LpcErrorSeverity::Warning) => std::cmp::Ordering::Greater,
                _ => std::cmp::Ordering::Equal,
            });

            // TODO: benchmark these type conversions vs `errors.remove(0)`
            let mut deq = VecDeque::from(errors);
            let mut e = deq.pop_front().unwrap();
            e = e.with_additional_errors(Vec::from(deq));
            return Err(e);
        }

        context
    }};
}

#[derive(Educe, Default, Builder)]
#[educe(Debug)]
#[builder(build_fn(error = "lpc_rs_errors::LpcError"))]
pub struct Compiler {
    /// The configuration to be used for this instance of the compiler
    #[builder(setter(into))]
    config: Rc<Config>,

    /// The current depth in the inheritance chain of this compiler
    #[builder(default)]
    inherit_depth: usize,

    /// Pointer to the simul_efuns to be used for this compilation
    #[builder(default)]
    #[educe(Debug(method = "qcell_process_option_debug"))]
    simul_efuns: Option<Rc<QCell<Process>>>,
}

impl Compiler {
    /// Fully compile a file into a Program struct
    ///
    /// # Arguments
    /// `path` - The full, on-server path of the file to compile. Also used for error messaging.
    ///  If the file in question ends in `.c`, the extension can be left off, per LPC convention.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::compiler::Compiler;
    /// use qcell::QCellOwner;
    ///
    /// let compiler = Compiler::default();
    /// let mut cell_key = QCellOwner::new();
    /// let prog = compiler
    ///     .compile_file("tests/fixtures/code/example.c", &mut cell_key)
    ///     .expect("Unable to compile.");
    /// ```
    #[instrument(skip(self, cell_key))]
    pub fn compile_file<T>(&self, path: T, cell_key: &mut QCellOwner) -> Result<Program>
    where
        T: Into<LpcPath> + Debug,
    {
        let lpc_path = path.into();
        let absolute = lpc_path.as_server(&*self.config.lib_dir);

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
                        self.compile_file(dot_c, cell_key)
                    }
                    _ => Err(LpcError::new(format!(
                        "Cannot read file `{}`: {}",
                        absolute.display(),
                        e
                    ))),
                };
            }
        };

        self.compile_string(lpc_path, file_content, cell_key)
    }

    /// Intended for in-game use to be able to compile a file with relative pathname handling
    #[instrument(skip(self, cell_key))]
    pub fn compile_in_game_file(
        &self,
        path: &LpcPath,
        span: Option<Span>,
        cell_key: &mut QCellOwner,
    ) -> Result<Program> {
        self.config.validate_in_game_path(path, span)?;

        self.compile_file(path, cell_key)
    }

    /// Take a str and preprocess it into a vector of Span tuples, and also
    /// returns the Preprocessor used.
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
    /// let (tokens, preprocessor) = compiler
    ///     .preprocess_string("~/my_file.c", code)
    ///     .expect("Failed to preprocess.");
    /// ```
    #[instrument(skip(self, code))]
    pub fn preprocess_string<P, S>(
        &self,
        path: P,
        code: S,
    ) -> Result<(Vec<Spanned<Token>>, Preprocessor)>
    where
        P: Into<LpcPath> + Debug,
        S: AsRef<str>,
    {
        let lpc_path = path.into();

        let context = CompilationContextBuilder::default()
            .filename(Arc::new(lpc_path.clone()))
            .config(self.config.clone())
            .inherit_depth(self.inherit_depth)
            .simul_efuns(self.simul_efuns.clone())
            .build()?;

        let mut preprocessor = Preprocessor::new(context);

        preprocessor
            .scan(&lpc_path, &code)
            .map(|tokens| (tokens, preprocessor))
    }

    /// Compile a string containing an LPC program into a Program struct
    ///
    /// # Arguments
    /// `path` - The absolute on-server path to the file being represented by `code`
    /// `code` - The actual code to be compiled.
    /// # Examples
    /// ```
    /// use lpc_rs::compiler::Compiler;
    /// use qcell::QCellOwner;
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
    /// let mut cell_key = QCellOwner::new();
    /// let prog = compiler
    ///     .compile_string("~/my_file.c", code, &mut cell_key)
    ///     .expect("Failed to compile.");
    /// ```
    #[instrument(skip_all)]
    pub fn compile_string<T, U>(
        &self,
        path: T,
        code: U,
        cell_key: &mut QCellOwner,
    ) -> Result<Program>
    where
        T: Into<LpcPath>,
        U: AsRef<str>,
    {
        let lpc_path = path.into();
        let (mut program_node, context) = self.parse_string(&lpc_path, code)?;

        // inject the auto-inherit if it's to be used.
        if let Some(dir) = &self.config.auto_inherit_file {
            let lpc_dir = LpcPath::new_in_game(dir.as_str(), "/", &*self.config.lib_dir);
            if lpc_dir != lpc_path {
                let node = InheritNode {
                    path: dir.to_string(),
                    namespace: None,
                    span: None,
                };

                program_node.inherits.insert(0, node);
            }
        }
        // println!("{:?}", program);

        // let mut printer = TreePrinter::new();
        // let _ = program.visit(&mut printer);

        let context = apply_walker!(InheritanceWalker, program_node, context, cell_key, true);
        let context = apply_walker!(
            FunctionPrototypeWalker,
            program_node,
            context,
            cell_key,
            false
        );
        let context = apply_walker!(ScopeWalker, program_node, context, cell_key, false);
        let context = apply_walker!(DefaultParamsWalker, program_node, context, cell_key, false);
        let context = apply_walker!(SemanticCheckWalker, program_node, context, cell_key, true);

        let mut asm_walker = CodegenWalker::new(context);

        program_node.visit(&mut asm_walker, cell_key)?;

        // emit warnings
        asm_walker
            .context()
            .errors
            .iter()
            .filter(|e| e.is_warning())
            .for_each(|e| {
                e.emit_diagnostics();
            });

        // for s in asm_walker.listing() {
        //     println!("{}", s);
        // }

        let program = match asm_walker.into_program() {
            Ok(p) => p,
            Err(e) => return Err(e),
        };

        println!("{}", program.filename);
        for s in program.listing() {
            println!("{s}");
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
        T: AsRef<str>,
    {
        let (tokens, preprocessor) = self.preprocess_string(path, code)?;

        let wrapper = TokenVecWrapper::new(&tokens);
        let mut context = preprocessor.into_context();

        lpc_parser::ProgramParser::new()
            .parse(&mut context, wrapper)
            .map(|p| (p, context))
            .map_err(LpcError::from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod test_compile_file {
        use super::*;

        #[test]
        fn tries_dot_c() {
            let mut cell_key = QCellOwner::new();
            let compiler = Compiler::default();

            assert!(compiler
                .compile_file("tests/fixtures/code/example", &mut cell_key)
                .is_ok());
        }
    }

    mod test_compile_in_game_file {
        use lpc_rs_utils::config::ConfigBuilder;

        use super::*;

        #[test]
        fn disallows_going_outside_the_root() {
            let mut cell_key = QCellOwner::new();
            let config: Rc<Config> = ConfigBuilder::default()
                .lib_dir("tests")
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default()
                .config(config.clone())
                .build()
                .unwrap();
            let server_path = LpcPath::new_server("../../secure.c");
            let in_game_path = LpcPath::new_in_game("../../secure.c", "/", &*config.lib_dir);

            assert!(compiler
                .compile_in_game_file(&server_path, None, &mut cell_key)
                .unwrap_err()
                .to_string()
                .starts_with("attempt to access a file outside of lib_dir"));

            assert!(compiler
                .compile_in_game_file(&in_game_path, None, &mut cell_key)
                .unwrap_err()
                .to_string()
                .starts_with("attempt to access a file outside of lib_dir"));
        }
    }

    mod test_compile_string {
        use lpc_rs_utils::config::ConfigBuilder;

        use super::*;

        #[test]
        fn uses_auto_inherit_if_specified() {
            let mut cell_key = QCellOwner::new();
            let config: Rc<Config> = ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .auto_inherit_file("/std/auto.c")
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default().config(config).build().unwrap();
            let code = r#"
                inherit "/std/object";

                string foo = auto_inherited();
            "#;
            let prog = compiler
                .compile_string("my_file.c", code, &mut cell_key)
                .unwrap();
            println!("{:?}", prog.functions.keys().collect::<Vec<_>>());
            let inherited = prog
                .functions
                .iter()
                .find(|(_, f)| f.name() == "auto_inherited")
                .unwrap();
            println!("{:?}", inherited);
            // assert!(prog.functions.keys().)
            // assert_eq!(prog.inherits.len(), 2);
            // assert_eq!(prog.inherits[0].filename.to_str().unwrap(), "/std/auto.c");
        }

        #[test]
        fn skips_auto_inherit_if_not_specified() {
            let mut cell_key = QCellOwner::new();
            let config: Rc<Config> = ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default().config(config).build().unwrap();
            let code = r#"
                inherit "/std/object";

                string foo = auto_inherited();
            "#;
            let err = compiler
                .compile_string("my_file.c", code, &mut cell_key)
                .unwrap_err();
            assert_eq!(
                &err.to_string(),
                "call to unknown function `auto_inherited`"
            );
        }
    }
}
