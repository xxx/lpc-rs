use std::{ffi::OsStr, fmt::Debug, io::ErrorKind, sync::Arc};

use ast::program_node::ProgramNode;
use async_recursion::async_recursion;
use codegen::{
    codegen_walker::CodegenWalker,
    function_prototype_walker::FunctionPrototypeWalker,
    inheritance_walker::InheritanceWalker,
    scope_walker::ScopeWalker,
    semantic_check_walker::SemanticCheckWalker,
    tree_walker::{ContextHolder, Pass, apply},
};
use compilation_context::CompilationContext;
use derive_builder::Builder;
use educe::Educe;
use lexer::{Token, TokenTriples};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{
    self, LpcError, Result, lpc_error,
    span::{HasSpan, Span},
};
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

/// A successful compile: the program, and every warning recorded on the
/// way — an inherited file's too — in recording order.
#[derive(Debug)]
pub struct Compiled {
    /// The compiled program.
    pub program: Program,
    /// The warnings; a compile that recorded an error fails instead.
    pub warnings: Vec<LpcError>,
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
    /// let compiled = Compiler::default()
    ///     .compile_file(LpcPath::new_server("tests/fixtures/code/example.c"))
    ///     .await
    ///     .expect("Unable to compile.");
    /// # });
    /// ```
    #[instrument(skip(self))]
    #[async_recursion]
    pub async fn compile_file<T>(&self, path: T) -> Result<Compiled>
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
    ) -> Result<Compiled> {
        self.config.validate_in_game_path(path, span)?;

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
    ) -> Result<(Vec<Token>, Preprocessor)>
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
            .scan(lpc_path, &code)
            .await
            .map(|tokens| (tokens, preprocessor))
    }

    /// Compile a string containing an LPC program into a [`Compiled`]
    /// program with its warnings.
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
    /// let compiled = compiler
    ///     .compile_string("~/my_file.c", code)
    ///     .await
    ///     .expect("Failed to compile.");
    /// # });
    /// ```
    #[instrument(skip_all)]
    pub async fn compile_string<T, U>(&self, path: T, code: U) -> Result<Compiled>
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

        let context = apply::<InheritanceWalker>(&mut program_node, context, true)
            .await?
            .into_context();
        let context = apply::<FunctionPrototypeWalker>(&mut program_node, context, false)
            .await?
            .into_context();
        let context = apply::<ScopeWalker>(&mut program_node, context, false)
            .await?
            .into_context();
        let context = apply::<SemanticCheckWalker>(&mut program_node, context, true)
            .await?
            .into_context();

        let mut asm_walker: CodegenWalker = apply(&mut program_node, context, true).await?;
        let warnings = asm_walker.diagnostics_mut().finish()?;
        let program = asm_walker.into_program()?;

        Ok(Compiled { program, warnings })
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
        let (tokens, preprocessor) = self.preprocess_string(path.clone(), code).await?;

        let wrapper = TokenTriples::new(&tokens);
        let mut context = preprocessor.into_context();

        lpc_parser::ProgramParser::new()
            .parse(&mut context, wrapper)
            .map(|p| (p, context))
            .map_err(|e| {
                // lalrpop's bare-usize locations carry no file id, so the
                // EOF/InvalidToken arms arrive span-less; every other arm
                // — `ParseError::User`'s grammar-action errors included —
                // already carries one.
                let last = tokens.last().map(|t| t.span());
                LpcError::from(e).or_span(last)
            })
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

            use crate::test_support::TempLib;

            let root = TempLib::new("stale-render");
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

            assert!(rendered.contains("int x = ;"), "{rendered}");
            assert!(!rendered.contains("rewritten"), "{rendered}");
        }
    }

    mod test_rendered_diagnostics {
        use indoc::indoc;

        use super::*;
        use crate::test_support::test_config;

        async fn rendered_error(code: &str) -> String {
            let compiler = Compiler::new(test_config());
            let e = compiler
                .compile_string("/my_file.c", code)
                .await
                .map(|_| ())
                .expect_err("expected a compile error");
            e.diagnostic_string()
        }

        #[tokio::test]
        async fn a_root_named_by_its_server_path_renders_in_game() {
            let config = test_config();
            let path = LpcPath::new_server(format!("{}/served.c", config.lib_dir));
            let e = Compiler::new(config)
                .compile_string(path, "int x = ;")
                .await
                .map(|_| ())
                .expect_err("expected a compile error");
            assert!(
                e.diagnostic_string().contains("┌─ /served.c:1:9"),
                "{}",
                e.diagnostic_string()
            );
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
                    "function prototypes are ignored; the definition declares the function",
                ]
            );
        }

        #[tokio::test]
        async fn a_string_slice_is_a_string_and_a_string_index_is_an_int() {
            let config: Arc<Config> = ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default().config(config).build().unwrap();
            let code = "void create() { string s = \"hello\"; string t = s[1..2]; int c = s[0]; }";

            let compiled = compiler.compile_string("/slice.c", code).await;

            assert!(
                compiled.is_ok(),
                "{}",
                compiled.unwrap_err().diagnostic_string()
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
            let prog = compiler
                .compile_string("my_file.c", code)
                .await
                .unwrap()
                .program;
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

    mod test_parse_errors {
        use lpc_rs_utils::config::ConfigBuilder;

        use super::*;
        use crate::{
            compiler::ast::{
                ast_node::{AstNode, SpannedNode},
                expression_node::ExpressionNode,
            },
            test_support::{TempLib, test_config},
        };

        #[tokio::test]
        async fn an_unexpected_eof_carries_the_last_tokens_span() {
            let compiler = Compiler::new(test_config());
            let e = compiler
                .compile_string("/eof.c", "int x = 1 +")
                .await
                .map(|_| ())
                .expect_err("expected a parse error");
            assert_eq!(e.to_string(), "Unexpected EOF");
            assert_eq!(e.span().and_then(|s| s.code()).as_deref(), Some("+"));
        }

        #[tokio::test]
        async fn a_cross_file_expression_span_stays_in_the_including_file() {
            let root = TempLib::new("cross-file");
            std::fs::write(root.join("two.h"), "2\n").unwrap();

            let config: Arc<Config> = ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .build()
                .unwrap()
                .into();
            let compiler = CompilerBuilder::default().config(config).build().unwrap();

            let code = "int x = y +\n#include \"two.h\"\n;\n";
            let path = LpcPath::new_in_game("/main.c", "/", root.to_str().unwrap());
            let (prog, _context) = compiler.parse_string(&path, code).await.unwrap();

            let AstNode::Decl(decl) = &prog.body[0] else {
                panic!("expected a decl");
            };
            let Some(ExpressionNode::BinaryOp(op)) = &decl.initializations[0].value else {
                panic!("expected a binary op");
            };
            let l_span = op.l.span().expect("lhs span");
            let r_span = op.r.span().expect("rhs span");
            assert_ne!(l_span.file_id(), r_span.file_id());
            // join's cross-file rule: the node keeps the left operand's span.
            assert_eq!(op.span, Some(l_span));
            assert_eq!(l_span.code().as_deref(), Some("y"));
        }
    }

    mod test_warnings {
        use super::*;
        use crate::test_support::test_config;

        #[tokio::test]
        async fn a_successful_compile_returns_its_warnings() {
            let compiler = Compiler::new(test_config());
            let compiled = compiler
                .compile_string("/w.c", "int f() { }")
                .await
                .unwrap();
            assert!(compiled.program.functions.values().any(|f| f.name() == "f"));
            let messages: Vec<_> = compiled.warnings.iter().map(|w| w.message()).collect();
            assert_eq!(
                messages,
                ["non-void function does not return a value. defaulting to 0."]
            );
        }

        #[tokio::test]
        async fn an_inherited_files_warnings_come_along() {
            let compiler = Compiler::new(test_config());
            let compiled = compiler
                .compile_string("/child.c", r#"inherit "/warns";"#)
                .await
                .unwrap();
            let rendered: Vec<_> = compiled
                .warnings
                .iter()
                .map(|w| w.diagnostic_string())
                .collect();
            assert_eq!(rendered.len(), 2, "{rendered:?}");
            assert!(rendered[0].contains("/warns.c:2:1"), "{}", rendered[0]);
            assert!(rendered[1].contains("/warns.c:5:1"), "{}", rendered[1]);
        }

        async fn warnings_of(code: &str) -> Vec<String> {
            let compiler = Compiler::new(test_config());
            let compiled = compiler.compile_string("/w.c", code).await.unwrap();
            compiled
                .warnings
                .iter()
                .map(|w| w.message().to_string())
                .collect()
        }

        #[tokio::test]
        async fn an_unused_local_is_a_warning() {
            assert_eq!(
                warnings_of("void f() { int unused; }").await,
                ["unused variable `unused`"]
            );
        }

        #[tokio::test]
        async fn an_unreachable_statement_is_a_warning() {
            assert_eq!(
                warnings_of("void f() { return; f(); }").await,
                ["unreachable statement"]
            );
        }

        #[tokio::test]
        async fn a_shadowing_local_is_a_warning() {
            assert_eq!(
                warnings_of("int g; void f(int g) { g++; }").await,
                ["`g` shadows a global variable"]
            );
        }

        #[tokio::test]
        async fn a_global_shadowing_an_inherited_global_is_a_warning() {
            assert_eq!(
                warnings_of(r#"inherit "/parent"; int b;"#).await,
                [
                    "`b` shadows a global inherited from `/grandparent.c`",
                    "`b` shadows a global inherited from `/parent.c`",
                ]
            );
        }

        #[tokio::test]
        async fn a_name_declared_by_two_parents_is_a_warning() {
            assert_eq!(
                warnings_of(r#"inherit "/twin_a"; inherit "/twin_b"; int f() { return twin; }"#)
                    .await,
                ["`twin` is declared by `/twin_a.c` and `/twin_b.c`; `/twin_b.c`'s is used"]
            );
        }
    }
}
