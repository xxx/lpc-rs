use std::{ffi::OsStr, fmt::Debug, io::ErrorKind, sync::Arc};

use ast::{ast_node::AstNodeTrait, program_node::ProgramNode};
use async_recursion::async_recursion;
use codegen::{
    codegen_walker::CodegenWalker, function_prototype_walker::FunctionPrototypeWalker,
    inheritance_walker::InheritanceWalker, scope_walker::ScopeWalker,
    semantic_check_walker::SemanticCheckWalker, tree_walker::ContextHolder,
};
use compilation_context::CompilationContext;
use derive_builder::Builder;
use educe::Educe;
use lexer::{Spanned, Token, TokenVecWrapper};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{self, LpcError, Result, lpc_error, span::Span};
use lpc_rs_utils::{config::Config, read_lpc_file};
use preprocessor::Preprocessor;
use tracing::instrument;
use ustr::ustr;

use crate::{
    compiler::{ast::inherit_node::InheritNode, compilation_context::CompilationContextBuilder},
    interpreter::{process::Process, program::Program},
    lpc_parser,
};

pub mod ast;
pub mod codegen;
pub mod compilation_context;
pub mod diagnostics;
pub mod lexer;
pub mod parser;
pub mod preprocessor;
pub mod semantic;

#[macro_export]
macro_rules! apply_walker {
    ($walker:ty, $program:expr, $context:expr, $fatal:expr) => {{
        let mut walker = <$walker>::new($context);
        let result = $program.visit(&mut walker).await;

        let mut context = walker.into_context();

        if let Err(e) = result {
            return Err(context.diagnostics.finish_with(e));
        }
        if $fatal && !context.diagnostics.is_clean() {
            return Err(context
                .diagnostics
                .finish()
                .expect_err("not clean, so finish fails"));
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
    config: Arc<Config>,

    /// The current depth in the inheritance chain of this compiler
    #[builder(default)]
    inherit_depth: u8,

    /// Pointer to the simul_efuns to be used for this compilation
    #[builder(default)]
    simul_efuns: Option<Arc<Process>>,
}

impl Compiler {
    /// Create a new [`Compiler`] with the passed [`Config`]
    pub fn new<C>(config: C) -> Self
    where
        C: Into<Arc<Config>>,
    {
        Self {
            config: config.into(),
            ..Default::default()
        }
    }

    /// Fully compile a file into a Program struct
    ///
    /// # Arguments
    /// `path` - The path of the file to compile. Also used for error messaging.
    ///  If the file in question ends in `.c`, the extension can be left off, per LPC convention.
    ///
    /// # Examples
    /// ```
    /// # tokio_test::block_on(async {
    /// use lpc_rs::compiler::Compiler;
    /// use lpc_rs_core::lpc_path::LpcPath;
    ///
    /// let prog = Compiler::default()
    ///     .compile_file(LpcPath::new_server("tests/fixtures/code/example.c"))
    ///     .await
    ///     .expect("Unable to compile.");
    /// # });
    /// ```
    #[instrument(skip(self))]
    #[async_recursion]
    pub async fn compile_file<T>(&self, path: T) -> Result<Program>
    where
        T: Into<LpcPath> + Debug + Send,
    {
        let lpc_path = path.into();
        let absolute = lpc_path.as_server(&*self.config.lib_dir);

        let file_content = match read_lpc_file(&*absolute).await {
            Ok(s) => s,
            Err(e) => {
                return match e.kind() {
                    ErrorKind::NotFound => {
                        if matches!(absolute.extension().and_then(OsStr::to_str), Some("c")) {
                            return Err(lpc_error!(
                                "Cannot read file `{}`: {}",
                                absolute.display(),
                                e
                            ));
                        }

                        let dot_c = lpc_path.with_extension("c");
                        self.compile_file(dot_c).await
                    }
                    _ => Err(lpc_error!(
                        "Cannot read file `{}`: {}",
                        absolute.display(),
                        e
                    )),
                };
            }
        };

        self.compile_string(lpc_path, file_content).await
    }

    /// Intended for in-game use to be able to compile a file with relative pathname handling
    #[instrument(skip(self))]
    pub async fn compile_in_game_file(
        &self,
        path: &LpcPath,
        span: Option<Span>,
    ) -> Result<Program> {
        self.config.validate_in_game_path(path, span)?;

        // owned here, not a reference: a &LpcPath would take the blanket `T: Into<PathBuf>` From impl and drop the Server variant
        self.compile_file(path.clone()).await
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
    /// # tokio_test::block_on(async {
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
    ///     .await
    ///     .expect("Failed to preprocess.");
    /// # });
    /// ```
    #[instrument(skip(self, code))]
    pub async fn preprocess_string<P, S>(
        &self,
        path: P,
        code: S,
    ) -> Result<(Vec<Spanned<Token>>, Preprocessor)>
    where
        P: Into<LpcPath> + Debug,
        S: AsRef<str> + Send + Sync,
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
            .await
            .map(|tokens| (tokens, preprocessor))
    }

    /// Compile a string containing an LPC program into a Program struct
    ///
    /// # Arguments
    /// `path` - The absolute on-server path to the file being represented by `code`
    /// `code` - The actual code to be compiled.
    /// # Examples
    /// ```
    /// # tokio_test::block_on(async {
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
    /// let prog = compiler
    ///     .compile_string("~/my_file.c", code)
    ///     .await
    ///     .expect("Failed to compile.");
    /// # });
    /// ```
    /// Where a successful compile's warnings go.
    async fn report_warnings(&self, warnings: Vec<LpcError>) {
        for warning in warnings {
            self.config.debug_log(warning.diagnostic_string()).await;
        }
    }

    #[instrument(skip_all)]
    pub async fn compile_string<T, U>(&self, path: T, code: U) -> Result<Program>
    where
        T: Into<LpcPath>,
        U: AsRef<str> + Send + Sync,
    {
        let lpc_path = path.into();
        let (mut program_node, context) = self.parse_string(&lpc_path, code).await?;

        // inject the auto-inherit if it's to be used.
        if let Some(dir) = &self.config.auto_inherit_file {
            let lpc_dir = LpcPath::new_in_game(dir.as_str(), "/", &*self.config.lib_dir);
            if lpc_dir != lpc_path {
                let node = InheritNode {
                    path: ustr(dir),
                    namespace: None,
                    span: None,
                };

                program_node.inherits.insert(0, node);
            }
        }
        // println!("{:?}", program);

        // let mut printer = TreePrinter::new();
        // let _ = program.visit(&mut printer);

        let context = apply_walker!(InheritanceWalker, program_node, context, true);
        let context = apply_walker!(FunctionPrototypeWalker, program_node, context, false);
        let context = apply_walker!(ScopeWalker, program_node, context, false);
        let context = apply_walker!(SemanticCheckWalker, program_node, context, true);

        let mut asm_walker = CodegenWalker::new(context);

        if let Err(e) = program_node.visit(&mut asm_walker).await {
            return Err(asm_walker.context_mut().diagnostics.finish_with(e));
        }
        let warnings = asm_walker.context_mut().diagnostics.finish()?;
        self.report_warnings(warnings).await;

        let program = match asm_walker.into_program() {
            Ok(p) => p,
            Err(e) => return Err(e),
        };

        // println!("{}", program.filename);
        // for s in program.listing() {
        //     println!("{s}");
        // }
        // println!();

        Ok(program)
    }

    /// Preprocess, then parse a string of code for the file at `path`
    ///
    /// # Returns
    /// A [`Result`] with a tuple containing the parsed [`ProgramNode`],
    /// as well as the [`Preprocessor`]'s [`CompilationContext`]
    #[instrument(skip(self, code))]
    pub async fn parse_string<T>(
        &self,
        path: &LpcPath,
        code: T,
    ) -> Result<(ProgramNode, CompilationContext)>
    where
        T: AsRef<str> + Send + Sync,
    {
        let (tokens, preprocessor) = self.preprocess_string(path, code).await?;

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
        use crate::test_support::test_config;

        #[tokio::test]
        async fn tries_dot_c() {
            let compiler = Compiler::new(test_config());

            assert!(compiler.compile_file("example").await.is_ok());
        }

        #[tokio::test]
        async fn a_diagnostic_renders_the_compiled_text_not_todays_disk_content() {
            use lpc_rs_utils::config::ConfigBuilder;

            let root =
                std::env::temp_dir().join(format!("lpc-rs-stale-render-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&root);
            std::fs::create_dir_all(&root).unwrap();
            std::fs::write(root.join("stale.c"), "int x = ;\n").unwrap();

            let config: Arc<Config> = ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default().config(config).build().unwrap();
            let e = compiler.compile_file("/stale.c").await.unwrap_err();

            std::fs::write(root.join("stale.c"), "// rewritten since the compile\n").unwrap();
            let rendered = e.diagnostic_string();
            let _ = std::fs::remove_dir_all(&root);

            assert!(rendered.contains("int x = ;"), "{rendered}");
            assert!(!rendered.contains("rewritten"), "{rendered}");
        }
    }

    mod test_rendered_diagnostics {
        use indoc::indoc;

        use super::*;
        use crate::test_support::{strip_lib_dir, test_config};

        async fn rendered_error(code: &str) -> String {
            let compiler = Compiler::new(test_config());
            let e = compiler
                .compile_string("/my_file.c", code)
                .await
                .map(|_| ())
                .expect_err("expected a compile error");
            strip_lib_dir(&e.diagnostic_string())
        }

        #[tokio::test]
        async fn a_compile_error_renders_with_its_label() {
            let code = indoc! { r#"
                nomask void noooo() {}
                void noooo() {}
            "# };
            assert_eq!(
                rendered_error(code).await,
                "error: attempt to redefine nomask function `noooo`\n  ┌─ /my_file.c:2:1\n  │\n1 │ nomask void noooo() {}\n  │ ----------------- defined here\n2 │ void noooo() {}\n  │ ^^^^^^^^^^\n\n"
            );
        }

        #[tokio::test]
        async fn an_error_in_an_included_file_names_that_file() {
            let code = indoc! { r#"
                #include "/include/bad_token.h"
            "# };
            assert_eq!(
                rendered_error(code).await,
                "error: Unrecognized Token: ;\n  ┌─ /include/bad_token.h:1:9\n  │\n1 │ int x = ;\n  │         ^\n  │\n  = expected one of: \"-\", \"!\", \"~\", \"&\", \"++\", \"--\", \"efun\", \"(\", \"::\", \"StringLiteral\", \"IntLiteral\", \"FloatLiteral\", \"ID\", \"ClosureArgVar\"\n\n"
            );
        }

        #[tokio::test]
        async fn a_label_into_an_inherited_file_renders_both_files() {
            let code = indoc! { r#"
                inherit "/nomask_parent";
                void noooo() {}
            "# };
            assert_eq!(
                rendered_error(code).await,
                "error: attempt to redefine nomask function `noooo`\n  ┌─ /my_file.c:2:1\n  │\n2 │ void noooo() {}\n  │ ^^^^^^^^^^\n  │\n  ┌─ /nomask_parent.c:1:1\n  │\n1 │ nomask void noooo() {\n  │ ----------------- defined here\n\n"
            );
        }
    }

    mod test_compile_in_game_file {
        use lpc_rs_utils::config::ConfigBuilder;

        use super::*;

        #[tokio::test]
        async fn disallows_going_outside_the_root() {
            let config: Arc<Config> = ConfigBuilder::default()
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

            assert!(
                compiler
                    .compile_in_game_file(&server_path, None)
                    .await
                    .unwrap_err()
                    .to_string()
                    .starts_with("attempt to access a file outside of lib_dir")
            );

            assert!(
                compiler
                    .compile_in_game_file(&in_game_path, None)
                    .await
                    .unwrap_err()
                    .to_string()
                    .starts_with("attempt to access a file outside of lib_dir")
            );
        }
    }

    mod test_compile_string {
        use lpc_rs_utils::config::ConfigBuilder;

        use super::*;

        #[tokio::test]
        async fn a_failed_compile_leads_with_the_error_and_keeps_the_warning_once() {
            let config: Arc<Config> = ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default().config(config).build().unwrap();
            let code = "int proto();\nvoid create() { 1++; }";

            let e = compiler.compile_string("/lead.c", code).await.unwrap_err();

            assert_eq!(e.to_string(), "Invalid operation on `int` literal");
            let rendered: Vec<_> = e.to_diagnostics().into_iter().map(|d| d.message).collect();
            assert_eq!(
                rendered,
                vec![
                    "Invalid operation on `int` literal",
                    "prototypes are ignored in this flavor of LPC",
                ]
            );
        }

        #[tokio::test]
        async fn a_recorded_error_leads_even_when_a_warning_came_first() {
            let config: Arc<Config> = ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default().config(config).build().unwrap();
            let code = "int proto();\nvoid create() { break; }";

            let e = compiler.compile_string("/lead.c", code).await.unwrap_err();

            assert_eq!(e.to_string(), "Invalid `break`.");
            assert!(!e.is_warning());
            assert_eq!(e.to_diagnostics().len(), 2);
        }

        #[tokio::test]
        async fn an_undefined_symbol_is_named() {
            let config: Arc<Config> = ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default().config(config).build().unwrap();

            let e = compiler
                .compile_string("/undefined.c", "void create() { x = 1; }")
                .await
                .unwrap_err();

            let rendered: Vec<_> = e.to_diagnostics().into_iter().map(|d| d.message).collect();
            assert_eq!(
                rendered,
                vec!["undefined variable `x`", "undefined symbol x"]
            );
        }

        #[tokio::test]
        async fn uses_auto_inherit_if_specified() {
            let config: Arc<Config> = ConfigBuilder::default()
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
            let prog = compiler.compile_string("my_file.c", code).await.unwrap();
            let _inherited = prog
                .functions
                .iter()
                .find(|(_, f)| f.name() == "auto_inherited")
                .unwrap();
            // assert!(prog.functions.keys().)
            // assert_eq!(prog.inherits.len(), 2);
            // assert_eq!(prog.inherits[0].filename.to_str().unwrap(), "/std/auto.c");
        }

        #[tokio::test]
        async fn skips_auto_inherit_if_not_specified() {
            let config: Arc<Config> = ConfigBuilder::default()
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
                .compile_string("my_file.c", code)
                .await
                .unwrap_err();
            assert_eq!(
                &err.to_string(),
                "call to unknown function `auto_inherited`"
            );
        }
    }
}
