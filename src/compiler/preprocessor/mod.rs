use std::collections::HashMap;

use async_recursion::async_recursion;
use define::{Define, ObjectMacro};
use lpc_rs_core::{
    LpcIntInner,
    lpc_path::LpcPath,
    pragma_flags::{NO_CLONE, NO_INHERIT, NO_SHADOW, RESIDENT, STRICT_TYPES},
};
use lpc_rs_errors::{
    LpcError, Result, lpc_error,
    source_map::FileId,
    span::{HasSpan, Span},
};
use tracing::{instrument, trace};

use crate::compiler::{
    ast::binary_op_node::BinaryOperation,
    compilation_context::CompilationContext,
    lexer::{LexWrapper, Token, logos_token::StringToken},
    preprocessor::preprocessor_node::PreprocessorNode,
};
use conditional::Conditionals;
use directive::{Directive, DirectiveKind};
use include::{IncludeSource, IncludeWalk, ONCE};

mod conditional;
pub mod define;
pub mod directive;
mod expand;
mod include;
pub mod preprocessor_node;

#[derive(Debug)]
pub struct Preprocessor {
    /// The compilation context
    context: CompilationContext,

    /// We keep track of `#define`d things here.
    defines: HashMap<String, Define>,

    /// The stack of open `#if`/`#ifdef`/`#ifndef`s, for the current file.
    conditionals: Conditionals,

    /// The include walk: `#include` traversal state for this compile.
    includes: IncludeWalk,
}

impl Preprocessor {
    /// Create a new `Preprocessor`
    ///
    /// # Arguments
    /// `context` - A context object to store data, errors, etc., generated
    /// during the compile
    ///
    /// # Examples
    /// ```
    /// use std::{rc::Rc, sync::Arc};
    ///
    /// use lpc_rs::compiler::{
    ///     compilation_context::{CompilationContext, CompilationContextBuilder},
    ///     preprocessor::Preprocessor,
    /// };
    /// use lpc_rs_utils::config::ConfigBuilder;
    ///
    /// let config = ConfigBuilder::default()
    ///     .lib_dir("/home/mud/lib")
    ///     .system_include_dirs(vec!["/include", "/sys"])
    ///     .build()
    ///     .unwrap();
    /// let context = CompilationContextBuilder::default()
    ///     .filename(Arc::new("test.c".into()))
    ///     .config(config)
    ///     .build()
    ///     .unwrap();
    /// let preprocessor = Preprocessor::new(context);
    /// ```
    #[instrument]
    pub fn new(context: CompilationContext) -> Self {
        Self {
            context,
            ..Self::default()
        }
    }

    /// Consume this preprocessor, and return its `Context`.
    ///
    /// This is intended for use after preprocessing has completed, and
    /// you're ready to re-take ownership of the context for the next step.
    #[instrument]
    pub fn into_context(self) -> CompilationContext {
        self.context
    }

    /// Scan a file's contents, transforming as necessary according to the
    /// preprocessing rules. This is the standard way to use the
    /// preprocessor
    ///
    /// # Arguments
    /// `path` - The in-game [`Path`]like representing the file.
    /// `code` - The code to scan.
    ///
    /// # Examples
    /// ```
    /// use std::{rc::Rc, sync::Arc};
    ///
    /// use lpc_rs::compiler::{
    ///     compilation_context::{CompilationContext, CompilationContextBuilder},
    ///     preprocessor::Preprocessor,
    /// };
    /// use lpc_rs_utils::config::ConfigBuilder;
    ///
    /// let config = ConfigBuilder::default()
    ///     .lib_dir("/home/mud/lib")
    ///     .system_include_dirs(vec!["/include", "/sys"])
    ///     .build()
    ///     .unwrap();
    /// let context = CompilationContextBuilder::default()
    ///     .filename(Arc::new("test.c".into()))
    ///     .config(config)
    ///     .build()
    ///     .unwrap();
    /// let mut preprocessor = Preprocessor::new(context);
    ///
    /// let code = r#"
    ///     #include "include/simple.h"
    ///
    ///     void main() {
    ///         int a = 123;
    ///     }
    /// "#;
    ///
    /// let processed = preprocessor.scan("foo.c", code);
    /// ```
    #[instrument(skip_all)]
    pub async fn scan<P, C>(&mut self, path: P, code: C) -> Result<Vec<Token>>
    where
        P: Into<LpcPath>,
        C: AsRef<str> + Send,
    {
        let mut output = vec![];

        let lpc_path = path.into();

        trace!("scanning {:?} :: {:?}", lpc_path, code.as_ref());

        let config = self.context.config.clone();
        let root_id = self.includes.open_root(&lpc_path, code.as_ref(), &config);

        // handle auto-include
        if let Some(auto_include) = &config.auto_include_file {
            let auto_include_path =
                LpcPath::new_server(format!("{}/{}", &config.lib_dir, auto_include));

            if auto_include_path != lpc_path {
                self.scan_include(
                    IncludeSource::Configured(&auto_include_path),
                    None,
                    &mut output,
                )
                .await?;
            }
        }

        let result = self.internal_scan(code, root_id, Some(output)).await;
        self.includes.close();
        result
    }

    /// The recursive function that takes care of scanning everything.
    #[async_recursion]
    async fn internal_scan<C>(
        &mut self,
        code: C,
        file_id: FileId,
        existing_output: Option<Vec<Token>>,
    ) -> Result<Vec<Token>>
    where
        C: AsRef<str> + Send,
    {
        let mut output = existing_output.unwrap_or_default();

        let src = code.as_ref();
        let token_stream = LexWrapper::new(src, file_id);

        let mut iter = token_stream.peekable();
        // The end of the last token this loop drew — the placement
        // check's anchor (spec R2).
        let mut prev_end: usize = 0;

        while let Some(next) = iter.next() {
            match next {
                Ok(token) => {
                    let mut end = token.span().r();

                    match &token {
                        Token::DirectiveLine(t) => {
                            self.handle_directive(t, prev_end, src, &mut output).await?;
                        }

                        // Handle macro expansion
                        Token::Id(st) => {
                            if self.conditionals.live() {
                                let appends = expand::Expander::new(&self.defines)
                                    .expand_use(st, &mut iter)?;

                                match appends {
                                    Some(mut expanded) => {
                                        output.append(&mut expanded.tokens);
                                        // Anchor placement past the whole use — the Id, or
                                        // through the top-level `)` the capture consumed (R4).
                                        end = expanded.use_span.r();
                                    }
                                    None => self.append(&mut output, token),
                                }
                            }
                        }
                        _ => self.append(&mut output, token),
                    }

                    prev_end = end;
                }
                Err(e) => {
                    if self.conditionals.live() {
                        return Err(e);
                    }
                    // Dead region: drop it — logos recovers per-token —
                    // but keep the anchor honest so a later mid-line
                    // directive on the same line isn't over-credited.
                    prev_end = e.span().map_or(prev_end, |s| s.r());
                }
            }
        }

        self.conditionals.finish()?;

        Ok(output)
    }

    /// One directive line: judge placement (R2), classify when dead (R3),
    /// parse and dispatch when live.
    async fn handle_directive(
        &mut self,
        token: &StringToken,
        prev_end: usize,
        src: &str,
        output: &mut Vec<Token>,
    ) -> Result<()> {
        let hash = token.0.l();
        // Well-placed iff a line start can be credited: the file starts
        // here, or a newline sits in the gap since the last token this
        // loop drew. Span ends never cover a newline (`track_slice`
        // trims `DirectiveLine`'s trailing grab; no other token's text
        // ends with one), so the newline a directive consumed is always
        // gap.
        let placed = prev_end == 0 || src[prev_end..hash].contains('\n');
        if !placed {
            if self.conditionals.live() {
                return Err(lpc_error!(
                    Some(token.0),
                    "preprocessor directives must appear on their own line.",
                ));
            }
            // Dead: a mid-line `#` is text, not a directive (C99 6.10.1).
            return Ok(());
        }

        if !self.conditionals.live() {
            // Dead regions know directive names, never operands.
            // `#else`/`#endif` have no operands — their shape-only
            // grammar is checked dead or live (spec R3).
            match directive::classify(&token.1) {
                DirectiveKind::If | DirectiveKind::IfDef | DirectiveKind::IfNDef => {
                    self.conditionals.enter(token.0, false);
                    return Ok(());
                }
                DirectiveKind::Else | DirectiveKind::Endif => {}
                _ => return Ok(()),
            }
        }

        match directive::parse(&token.1, token.0)? {
            Directive::Include { path, sys } => {
                let source = if sys {
                    IncludeSource::System { path: &path }
                } else {
                    IncludeSource::Local { path: &path }
                };
                self.scan_include(source, Some(token.0), output).await
            }
            Directive::Define {
                name,
                params,
                body,
                body_span,
            } => self.handle_define(token.0, name, params, body, body_span),
            Directive::Undef { name } => {
                self.defines.remove(&name);
                Ok(())
            }
            Directive::If { expr } => {
                let printing_lines =
                    self.eval_expr_for_skipping(&expr, Some(token.0), &mut Vec::new())?;
                self.conditionals.enter(token.0, printing_lines);
                Ok(())
            }
            Directive::IfDef { name } => {
                let taken = self.defines.contains_key(&name);
                self.conditionals.enter(token.0, taken);
                Ok(())
            }
            Directive::IfNDef { name } => {
                let taken = !self.defines.contains_key(&name);
                self.conditionals.enter(token.0, taken);
                Ok(())
            }
            Directive::Else => self.conditionals.flip_else(token.0),
            Directive::Endif => self.conditionals.leave(token.0),
            Directive::Pragma { names } => self.handle_pragma(token.0, names),
            Directive::Null => Ok(()),
        }
    }

    /// Add a parsed `#define` — object or function-style — to the
    /// defines table.
    #[instrument(skip(self))]
    fn handle_define(
        &mut self,
        span: Span,
        name: String,
        params: Option<Vec<String>>,
        body: String,
        body_span: Span,
    ) -> Result<()> {
        if self.defines.contains_key(&name) {
            return Err(
                LpcError::new(format!("duplicate `#define`: `{name}`")).with_span(Some(span))
            );
        }

        // Lex the body in place — tokens are born with their true
        // definition-site spans (card ④ R3). A directive line inside a
        // body has no legal reading (R6 — LPC has no `#` operator).
        let lex_body = |body: &str| -> Result<Vec<Token>> {
            let tokens = LexWrapper::new_at(body, body_span.file_id(), body_span.l())
                .collect::<Result<Vec<_>>>()?;
            if let Some(t) = tokens.iter().find(|t| matches!(t, Token::DirectiveLine(_))) {
                return Err(lpc_error!(
                    Some(t.span()),
                    "a preprocessor directive cannot appear in a macro body",
                ));
            }
            Ok(tokens)
        };

        let define = if let Some(args) = params {
            Define::new_function(lex_body(&body)?, args)
        } else if body.is_empty() {
            // A bare `#define FOO` expands to nothing and is no
            // expression (card ② R13).
            Define::new_object(vec![], None)
        } else {
            let tokens = lex_body(&body)?;
            // A body that is not an expression is still fine to
            // substitute; only an `#if` over it is an error.
            let expr = directive::parse_if_expression(&body, body_span).ok();
            Define::new_object(tokens, expr)
        };

        self.defines.insert(name, define);
        Ok(())
    }

    fn not_an_expression(name: &str, span: Option<Span>) -> LpcError {
        lpc_error!(
            span,
            "`{}` does not expand to a preprocessor expression",
            name
        )
    }

    /// One include: open through the walk, scan the file with its own
    /// conditional stack, close on the success and error paths alike.
    #[instrument(skip(self, output))]
    async fn scan_include(
        &mut self,
        source: IncludeSource<'_>,
        span: Option<Span>,
        output: &mut Vec<Token>,
    ) -> Result<()> {
        let config = self.context.config.clone();
        let Some(opened) = self.includes.open(source, span, &config).await? else {
            return Ok(());
        };

        // An included file's `#if`s must not leak into the includer's
        // conditional stack.
        debug_assert!(self.conditionals.live());
        let saved = std::mem::take(&mut self.conditionals);
        let result = self
            .internal_scan(opened.content, opened.file_id, None)
            .await;
        self.conditionals = saved;
        self.includes.close();

        for token in result? {
            self.append(output, token)
        }
        Ok(())
    }

    /// Determine if a particular node will enable line skipping or not.
    /// Returns `true` if we should print lines, and `false` if they should be
    /// skipped.
    ///
    /// `hide` tracks the names currently being resolved, so a name that
    /// refers back to itself (directly or mutually) resolves as undefined
    /// rather than recursing forever.
    #[instrument(skip(self, expr, hide))]
    fn eval_expr_for_skipping<'a>(
        &'a self,
        expr: &'a PreprocessorNode,
        span: Option<Span>,
        hide: &mut Vec<&'a str>,
    ) -> Result<bool> {
        match expr {
            PreprocessorNode::Var(x) => {
                if hide.contains(&x.as_str()) {
                    return Ok(false); // hidden name reads as undefined
                }
                match self.defines.get(x) {
                    Some(Define::Object(ObjectMacro {
                        expr: Some(expr), ..
                    })) => {
                        if hide.len() >= expand::MAX_EXPANSION_DEPTH {
                            return Err(lpc_error!(
                                span,
                                "expansion of `{}` nests too deeply (limit {})",
                                x,
                                expand::MAX_EXPANSION_DEPTH
                            ));
                        }
                        hide.push(x);
                        let result = self.eval_expr_for_skipping(expr, span, hide);
                        hide.pop();
                        result
                    }
                    Some(Define::Object(ObjectMacro { expr: None, .. })) => {
                        Err(Self::not_an_expression(x, span))
                    }
                    _ => Ok(false),
                }
            }
            PreprocessorNode::Int(i) => Ok(i != &0),
            PreprocessorNode::String(_) => Ok(true),
            PreprocessorNode::Defined(x, negated) => {
                let option = self.defines.get(x);
                Ok(if *negated {
                    option.is_none()
                } else {
                    option.is_some()
                })
            }
            PreprocessorNode::BinaryOp(op, l, r) => match op {
                BinaryOperation::Add => Ok(self
                    .resolve_int(l, span, hide)?
                    .wrapping_add(self.resolve_int(r, span, hide)?)
                    != 0),
                BinaryOperation::Sub => Ok(self
                    .resolve_int(l, span, hide)?
                    .wrapping_sub(self.resolve_int(r, span, hide)?)
                    != 0),
                BinaryOperation::AndAnd => Ok(self.eval_expr_for_skipping(l, span, hide)?
                    && self.eval_expr_for_skipping(r, span, hide)?),
                BinaryOperation::OrOr => Ok(self.eval_expr_for_skipping(l, span, hide)?
                    || self.eval_expr_for_skipping(r, span, hide)?),
                op => Err(lpc_error!(
                    span,
                    "unknown binary operation `{}` in expression `{}`",
                    op,
                    expr
                )),
            },
        }
    }

    /// Resolve a [`PreprocessorNode`] to an Int if possible.
    ///
    /// `hide` tracks the names currently being resolved, so a name that
    /// refers back to itself (directly or mutually) errors instead of
    /// recursing forever.
    #[instrument(skip(self, expr, hide))]
    fn resolve_int<'a>(
        &'a self,
        expr: &'a PreprocessorNode,
        span: Option<Span>,
        hide: &mut Vec<&'a str>,
    ) -> Result<LpcIntInner> {
        match expr {
            PreprocessorNode::Var(x) => {
                if hide.contains(&x.as_str()) {
                    // hidden name reads as undefined
                    return Err(lpc_error!(span, "unable to resolve into an int: `{}`", x));
                }
                if let Some(val) = self.defines.get(x) {
                    match val {
                        Define::Object(ObjectMacro {
                            expr: Some(expr), ..
                        }) => {
                            if hide.len() >= expand::MAX_EXPANSION_DEPTH {
                                return Err(lpc_error!(
                                    span,
                                    "expansion of `{}` nests too deeply (limit {})",
                                    x,
                                    expand::MAX_EXPANSION_DEPTH
                                ));
                            }
                            hide.push(x);
                            let result = self.resolve_int(expr, span, hide);
                            hide.pop();
                            result
                        }
                        Define::Object(ObjectMacro { expr: None, .. }) => {
                            Err(Self::not_an_expression(x, span))
                        }
                        Define::Function(_) => Ok(0),
                    }
                } else {
                    Err(lpc_error!(span, "unable to resolve into an int: `{}`", x))
                }
            }
            PreprocessorNode::Int(i) => Ok(*i),
            PreprocessorNode::Defined(x, negated) => {
                Ok((self.defines.contains_key(x) != *negated) as LpcIntInner)
            }
            PreprocessorNode::BinaryOp(op, l, r) => {
                let li = self.resolve_int(l, span, hide)?;
                let ri = self.resolve_int(r, span, hide)?;

                match op {
                    BinaryOperation::Add => Ok(li.wrapping_add(ri)),
                    BinaryOperation::Sub => Ok(li.wrapping_sub(ri)),
                    BinaryOperation::AndAnd => Ok(((li != 0) && (ri != 0)) as LpcIntInner),
                    BinaryOperation::OrOr => Ok(((li != 0) || (ri != 0)) as LpcIntInner),

                    operation => Err(lpc_error!(
                        span,
                        "unknown binary operation `{}` in expression `{}`",
                        operation,
                        expr
                    )),
                }
            }
            _ => Err(lpc_error!(
                span,
                "attempt to convert unknown node type to int: `{}`",
                expr
            )),
        }
    }

    /// Apply a parsed `#pragma`'s names. Which names exist is semantic,
    /// so the unknown-pragma error lives here, not in the grammar.
    #[instrument(skip(self))]
    fn handle_pragma(&mut self, span: Span, names: Vec<String>) -> Result<()> {
        for arg in names {
            match arg.as_str() {
                ONCE => self.includes.mark_once(),
                NO_CLONE => self.context.pragmas.set_no_clone(true),
                NO_INHERIT => self.context.pragmas.set_no_inherit(true),
                NO_SHADOW => self.context.pragmas.set_no_shadow(true),
                RESIDENT => self.context.pragmas.set_resident(true),
                STRICT_TYPES => self.context.pragmas.set_strict_types(true),
                x => {
                    return Err(lpc_error!(Some(span), "unknown pragma `{}`", x));
                }
            }
        }

        Ok(())
    }

    /// skip-aware way to append to the output
    #[inline]
    #[instrument(skip(self, output, token))]
    fn append(&self, output: &mut Vec<Token>, token: Token) {
        if self.conditionals.live() {
            output.push(token);
        }
    }
}

impl Default for Preprocessor {
    #[instrument]
    fn default() -> Self {
        Self {
            context: CompilationContext::default(),
            defines: HashMap::new(),
            conditionals: Conditionals::default(),
            includes: IncludeWalk::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::{assert_regex, compiler::compilation_context::CompilationContextBuilder};

    fn fixture() -> Preprocessor {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .system_include_dirs(vec!["/sys", "sys2"])
            .auto_include_file("/include/auto.h")
            .build()
            .unwrap();

        let context = CompilationContextBuilder::default()
            .filename(Arc::new("test.c".into()))
            .config(config)
            .build()
            .unwrap();
        Preprocessor::new(context)
    }

    async fn test_valid(input: &str, expected: &[&str]) {
        let mut preprocessor = fixture();
        match preprocessor.scan("/test.c", input).await {
            Ok(result) => {
                let mapped = result.iter().map(|i| i.to_string()).collect::<Vec<_>>();

                assert_eq!(mapped, expected)
            }
            Err(e) => {
                panic!("{e:?}")
            }
        }
    }

    // `expected` is converted to a Regex, for easier matching on errors.
    async fn test_invalid(input: &str, expected: &str) {
        let mut preprocessor = fixture();
        match preprocessor.scan("/test.c", input).await {
            Ok(result) => {
                panic!("Expected to fail, but passed with {result:?}");
            }
            Err(e) => {
                assert_regex!(e.message(), expected);
            }
        }
    }

    #[tokio::test]
    async fn test_ignored_if_commented() {
        let input = indoc! { r#"
                /* #defoon laksdjfalskdj */
                // #if 0
                    "This should be printed"
                // #endif
            "# };

        test_valid(input, &["This should be printed"]).await;
    }

    #[tokio::test]
    async fn test_auto_include() {
        let input = indoc! { r#"
                string marf = MY_FN;
            "# };

        test_valid(
            input,
            &[
                "string",
                "marf",
                "=",
                "file_name",
                "(",
                "efun",
                "::",
                "this_object",
                "(",
                ")",
                ")",
                ";",
            ],
        )
        .await;
    }

    mod test_system_includes {
        use super::*;

        #[tokio::test]
        async fn test_includes_the_file() {
            let input = r#"#include <sys_include1.h>"#;

            let expected = vec!["sys_include1.h"];

            test_valid(input, &expected).await;
        }

        #[tokio::test]
        async fn test_includes_multiple_levels() {
            let input = r#"#include <sys_include2.h>"#;

            let expected = vec!["sys_include1.h", "sys_include2.h"];

            test_valid(input, &expected).await;
        }

        #[tokio::test]
        async fn test_includes_multiple_files() {
            let input = indoc! {r#"
                #include <sys_include2.h>
                int j = 123;
                #include <sys_include1.h>
            "#};

            let expected = vec![
                "sys_include1.h",
                "sys_include2.h",
                "int",
                "j",
                "=",
                "123",
                ";",
                "sys_include1.h",
            ];

            test_valid(input, &expected).await;
        }

        #[tokio::test]
        async fn test_ifdefed_out() {
            let input = indoc! { r#"
                #ifdef FOO
                #include <sys_include1.h>
                #include <nonexistent.h>
                #endif
            "# };

            test_valid(input, &[]).await;
        }

        #[tokio::test]
        async fn test_errors_for_nonexistent_paths() {
            let input = r#"#include <nonexistent.h>"#;

            test_invalid(
                input,
                "^unable to read include file `/nonexistent.h`: No such file or directory \\(os error 2\\)$",
            )
            .await;
        }

        #[tokio::test]
        async fn an_unreadable_system_header_is_an_error_not_a_fallthrough() {
            use std::os::unix::fs::PermissionsExt;

            let root = std::env::temp_dir()
                .join(format!("lpc-rs-unreadable-header-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&root);
            std::fs::create_dir_all(root.join("sys")).unwrap();
            std::fs::create_dir_all(root.join("sys2")).unwrap();
            std::fs::write(root.join("sys/secret.h"), "first").unwrap();
            std::fs::write(root.join("sys2/secret.h"), "second").unwrap();
            std::fs::set_permissions(
                root.join("sys/secret.h"),
                std::fs::Permissions::from_mode(0o000),
            )
            .unwrap();

            let config = ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .system_include_dirs(vec!["/sys", "/sys2"])
                .build()
                .unwrap();
            let context = CompilationContextBuilder::default()
                .filename(Arc::new("test.c".into()))
                .config(config)
                .build()
                .unwrap();
            let mut preprocessor = Preprocessor::new(context);

            let result = preprocessor.scan("/test.c", "#include <secret.h>").await;
            let _ = std::fs::remove_dir_all(&root);

            let e = result.unwrap_err();
            assert_eq!(
                e.message(),
                "unable to read include file `/sys/secret.h`: Permission denied (os error 13)"
            );
        }

        #[tokio::test]
        async fn test_errors_for_traversal_attacks() {
            let input = r#"#include </../../some_file.h>"#;

            test_invalid(input, "attempt to include a file outside the root").await;
        }

        #[tokio::test]
        async fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #include <sys_include1.h>
            "#
            };

            test_invalid(
                prog,
                "preprocessor directives must appear on their own line",
            )
            .await;
        }

        #[tokio::test]
        async fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #include <sys_include1.h> klasjd
            "#
            };

            test_invalid(prog, "unexpected tokens after `#include`").await;
        }
    }

    mod test_local_includes {
        use super::*;

        #[tokio::test]
        async fn test_includes_the_file() {
            let input = r#"#include "include/simple.h""#;

            let expected = vec!["1", "+", "2", "+", "3", "+", "4", "+", "5", ";"];

            test_valid(input, &expected).await;
        }

        #[tokio::test]
        async fn test_includes_multiple_levels() {
            let input = r#"#include "include/level_2/two_level.h""#;

            let expected = vec!["1", "+", "2", "+", "3", "+", "4", "+", "5", ";"];

            test_valid(input, &expected).await;
        }

        #[tokio::test]
        async fn test_includes_multiple_files() {
            let input = indoc! {r#"
                #include "include/level_2/two_level.h"
                int j = 123;
                #include "include/simple.h"
            "#};

            let expected = vec![
                "1", "+", "2", "+", "3", "+", "4", "+", "5", ";", "int", "j", "=", "123", ";", "1",
                "+", "2", "+", "3", "+", "4", "+", "5", ";",
            ];

            test_valid(input, &expected).await;
        }

        #[tokio::test]
        async fn test_includes_absolute_paths() {
            let input = r#"#include "/include/simple.h""#;

            let expected = vec!["1", "+", "2", "+", "3", "+", "4", "+", "5", ";"];

            test_valid(input, &expected).await;
        }

        #[tokio::test]
        async fn a_reincluded_header_keeps_one_file_id() {
            // No auto-include: the token list must be exactly two copies.
            let config = ConfigBuilder::default()
                .lib_dir("./tests/fixtures/code")
                .build()
                .unwrap();
            let context = CompilationContextBuilder::default()
                .filename(Arc::new("test.c".into()))
                .config(config)
                .build()
                .unwrap();
            let mut preprocessor = Preprocessor::new(context);

            let input = indoc! { r#"
                #include "include/simple.h"
                #include "include/simple.h"
            "# };
            let result = preprocessor.scan("/test.c", input).await.unwrap();

            assert!(!result.is_empty(), "simple.h holds tokens");
            assert_eq!(result.len() % 2, 0);
            let (first, second) = result.split_at(result.len() / 2);
            for (a, b) in first.iter().zip(second) {
                assert_eq!(a.to_string(), b.to_string());
                assert_eq!(a.span().file_id(), b.span().file_id());
            }
        }

        #[tokio::test]
        async fn test_ifdefed_out() {
            let input = indoc! { r#"
                #ifdef FOO
                #include "./simple.h"
                #endif
            "# };

            test_valid(input, &[]).await;
        }

        #[tokio::test]
        async fn test_errors_for_nonexistent_paths() {
            let input = r#"#include "/askdf/foo.h""#;

            test_invalid(
                input,
                "^unable to read include file `/askdf/foo.h`: No such file or directory \\(os error 2\\)$",
            )
            .await;
        }

        #[tokio::test]
        async fn test_errors_for_traversal_attacks() {
            let input = r#"#include "/../../some_file.h""#;

            test_invalid(input, "attempt to include a file outside the root").await;
        }

        #[tokio::test]
        async fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #include "foo.h"
            "#
            };

            test_invalid(
                prog,
                "preprocessor directives must appear on their own line",
            )
            .await;
        }

        #[tokio::test]
        async fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #include "./include/simple.h" klasjd
            "#
            };

            test_invalid(prog, "unexpected tokens after `#include`").await;
        }
    }

    mod test_defines {
        use claims::assert_matches;

        use super::*;

        #[tokio::test]
        async fn test_object_define() {
            let input = indoc! { r#"
                #define ASS 1234
                #define MAR
                #define DOOD 666 + MAR
                #define SNUH 0x123
                #define TO this_object()
            "# };
            let mut preprocessor = fixture();

            match preprocessor.scan("test.c", input).await {
                Ok(_) => {
                    assert!(matches!(
                        preprocessor.defines.get("ASS").unwrap(),
                        Define::Object(ObjectMacro {
                            expr: Some(PreprocessorNode::Int(1234)),
                            ..
                        })
                    ));
                    assert!(matches!(
                        preprocessor.defines.get("MAR").unwrap(),
                        Define::Object(ObjectMacro { expr: None, .. })
                    ));
                    if let Define::Object(ObjectMacro { expr, .. }) =
                        preprocessor.defines.get("DOOD").unwrap()
                    {
                        assert_eq!(
                            expr.as_ref(),
                            Some(&PreprocessorNode::BinaryOp(
                                BinaryOperation::Add,
                                Box::new(PreprocessorNode::Int(666)),
                                Box::new(PreprocessorNode::Var(String::from("MAR")))
                            ))
                        );
                    } else {
                        panic!("Failed to match.")
                    }
                    assert_matches!(
                        preprocessor.defines.get("SNUH").unwrap(),
                        Define::Object(ObjectMacro {
                            expr: Some(PreprocessorNode::Int(291)),
                            ..
                        })
                    );
                    assert_matches!(
                        preprocessor.defines.get("TO").unwrap(),
                        Define::Object(ObjectMacro { expr: None, .. })
                    );
                }
                Err(e) => {
                    panic!("{e:?}")
                }
            }
        }

        #[tokio::test]
        async fn test_duplicate_define() {
            let input = indoc! { r#"
                #define ASS 123
                #define ASS 456
            "# };
            let mut preprocessor = fixture();

            match preprocessor.scan("test.c", input).await {
                Ok(_) => {
                    panic!("Expected an error due to duplicate definition.");
                }
                Err(e) => {
                    assert_eq!(e.to_string(), "duplicate `#define`: `ASS`");
                }
            }
        }

        #[tokio::test]
        async fn test_duplicate_after_undef() {
            let input = indoc! { r#"
                #define ASS 123
                #undef ASS
                #define ASS 456
            "# };
            let mut preprocessor = fixture();

            match preprocessor.scan("test.c", input).await {
                Ok(_) => {
                    assert!(matches!(
                        preprocessor.defines.get("ASS").unwrap(),
                        Define::Object(ObjectMacro {
                            expr: Some(PreprocessorNode::Int(456)),
                            ..
                        })
                    ));
                }
                Err(e) => {
                    panic!("{e:?}")
                }
            }
        }

        #[tokio::test]
        async fn test_duplicate_ifdefed_out() {
            let input = indoc! { r#"
                #define HELLO 123
                #ifdef FOO
                #define HELLO 456
                #endif
            "# };
            let mut preprocessor = fixture();

            match preprocessor.scan("test.c", input).await {
                Ok(_) => {
                    assert!(matches!(
                        preprocessor.defines.get("HELLO").unwrap(),
                        Define::Object(ObjectMacro {
                            expr: Some(PreprocessorNode::Int(123)),
                            ..
                        })
                    ));
                }
                Err(e) => {
                    panic!("{e:?}")
                }
            }
        }

        #[tokio::test]
        async fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #define LOL WUT
            "#
            };

            test_invalid(
                prog,
                "preprocessor directives must appear on their own line",
            )
            .await;
        }

        #[tokio::test]
        async fn consecutive_directive_lines_are_each_well_placed() {
            // The first grab's consumed newline sits in the gap, where the
            // placement check sees it.
            let prog = indoc! { r#"
                #define A 1
                #define B 2
                A + B;
            "# };
            test_valid(prog, &["1", "+", "2", ";"]).await;
        }

        #[tokio::test]
        async fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #define
            "#
            };

            test_invalid(prog, "expected an identifier after `#define`").await;
        }

        #[tokio::test]
        async fn a_directive_in_a_macro_body_errors_at_define_time() {
            // Stored silently before; it leaked to the parser at use (R6).
            test_invalid(
                "#define X #include \"foo.h\"\n",
                "a preprocessor directive cannot appear in a macro body",
            )
            .await;
        }

        #[tokio::test]
        async fn test_empty_define_expands_to_nothing() {
            let prog = indoc! { r##"
                #define FOO
                marf FOO tarf
            "## };

            test_valid(prog, &["marf", "tarf"]).await;
        }

        #[tokio::test]
        async fn test_empty_define_is_not_an_expression() {
            let prog = indoc! { r##"
                #define FOO
                #if FOO
                #endif
            "## };

            test_invalid(prog, "`FOO` does not expand to a preprocessor expression").await;
        }

        #[tokio::test]
        async fn test_empty_define_is_defined() {
            let prog = indoc! { r##"
                #define FOO
                #ifdef FOO
                "yes"
                #endif
            "## };

            test_valid(prog, &["yes"]).await;
        }
    }

    mod test_ifdef {
        use super::*;

        #[tokio::test]
        async fn test_with_defined() {
            let prog = indoc! { r#"
                #define FOO
                #ifdef FOO
                I should be rendered
                #endif
                #ifdef BAR
                I should not be rendered
                #endif
                #undef FOO
                #ifdef FOO
                I also should not be rendered
                #endif
            "# };

            let expected = vec!["I", "should", "be", "rendered"];

            test_valid(prog, &expected).await;
        }

        #[tokio::test]
        async fn test_ifdefed_out() {
            let input = indoc! { r#"
                #define BAR
                #ifdef FOO
                #ifdef BAR
                i should not be rendered
                #endif
                #endif
            "# };

            test_valid(input, &[]).await;
        }

        #[tokio::test]
        async fn test_error_without_if() {
            let prog = indoc! { r#"
                #define FOO
                "this will error because of the #endif without an #if or #ifdef";
                #endif
            "# };

            test_invalid(prog, "found `#endif` without a corresponding `#if`").await;
        }

        #[tokio::test]
        async fn test_error_without_endif() {
            let prog = indoc! { r#"
                #define FOO
                #ifdef FOO
                "this will error because there's no endif";
            "# };

            test_invalid(prog, "Found `#if` without a corresponding `#endif`").await;
        }

        #[tokio::test]
        async fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #ifdef WUT
            "#
            };

            test_invalid(
                prog,
                "preprocessor directives must appear on their own line",
            )
            .await;
        }

        #[tokio::test]
        async fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #ifdef
                123;
                #endif
            "#
            };

            test_invalid(prog, "expected an identifier after `#ifdef`").await;
        }
    }

    mod test_ifndef {
        use super::*;

        #[tokio::test]
        async fn test_with_not_defined() {
            let prog = indoc! { r#"
                #define BAR
                #ifndef FOO
                I should be rendered
                #endif
                #ifndef BAR
                I should not be rendered
                #endif
                #define FOO
                #ifndef FOO
                I also should not be rendered
                #endif
            "# };

            let expected = vec!["I", "should", "be", "rendered"];

            test_valid(prog, &expected).await;
        }

        #[tokio::test]
        async fn test_ifdefed_out() {
            let input = indoc! { r#"
                #ifdef FOO
                #ifndef BAR
                i should not be rendered
                #endif
                #endif
            "# };

            test_valid(input, &[]).await;
        }

        #[tokio::test]
        async fn test_error_without_endif() {
            let prog = indoc! { r#"
                #ifndef FOO
                "this will error because there's no endif";
            "# };

            test_invalid(prog, "Found `#if` without a corresponding `#endif`").await;
        }

        #[tokio::test]
        async fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #ifndef HELLO
                1 + 3;
            "#
            };

            test_invalid(
                prog,
                "preprocessor directives must appear on their own line",
            )
            .await;
        }

        #[tokio::test]
        async fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #ifndef
                123;
                #endif
            "#
            };

            test_invalid(prog, "expected an identifier after `#ifndef`").await;
        }
    }

    mod test_else {
        use super::*;

        #[tokio::test]
        async fn test_else() {
            let prog = indoc! { r#"
                #define FOO
                #ifdef FOO
                I should be rendered 1
                #else
                I should not be rendered 1
                #endif
                #ifndef FOO
                I should not be rendered 2
                #else
                I should be rendered 2
                #endif
                #undef FOO
                #ifndef FOO
                I should be rendered 3
                #else
                I should not be rendered 3
                #endif
            "# };

            let expected = vec![
                "I", "should", "be", "rendered", "1", "I", "should", "be", "rendered", "2", "I",
                "should", "be", "rendered", "3",
            ];

            test_valid(prog, &expected).await;
        }

        #[tokio::test]
        async fn test_ifdefed_out() {
            let input = indoc! { r#"
                #ifdef FOO
                #ifndef BAR
                i should not be rendered
                #else
                i also should not be rendered
                #endif
                #endif
            "# };

            test_valid(input, &[]).await;
        }

        #[tokio::test]
        async fn test_error_on_duplicate_else() {
            let prog = indoc! { r#"
                #ifndef FOO
                #else
                "this will error because of the duplicate #else";
                #else
                #endif
            "# };

            test_invalid(prog, "duplicate `#else`").await;
        }

        #[tokio::test]
        async fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #else
            "#
            };

            test_invalid(
                prog,
                "preprocessor directives must appear on their own line",
            )
            .await;
        }

        #[tokio::test]
        async fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #ifdef ASD
                #else 1 + 4
                #endif
            "#
            };

            test_invalid(prog, "unexpected tokens after `#else`").await;
        }

        #[tokio::test]
        async fn a_trailing_endif_operand_is_an_error() {
            // Silently accepted before the directive grammar: the `#endif`
            // re-check regex was commented out.
            test_invalid(
                "#ifdef FOO\n#endif garbage\n",
                "unexpected tokens after `#endif`",
            )
            .await;
        }

        #[tokio::test]
        async fn an_else_comment_is_whitespace() {
            // An error before the directive grammar; C strips comments first.
            let prog = indoc! { r#"
                #ifdef NOPE
                "dead"
                #else /* why we are here */
                "live"
                #endif
            "# };
            test_valid(prog, &["live"]).await;
        }
    }

    mod test_object_expansion {
        use super::*;

        #[tokio::test]
        async fn test_simple_replacement() {
            let prog = indoc! { r#"
                #define FOO 666

                int a = 1 + 5 + FOO + 3;
            "# };

            let expected = vec!["int", "a", "=", "1", "+", "5", "+", "666", "+", "3", ";"];

            test_valid(prog, &expected).await;
        }

        #[tokio::test]
        async fn test_multi_token_replacement() {
            let prog = indoc! { r#"
                #define FOO 666 + 54

                int a = 1 + 5 + FOO + 3;
            "# };

            let expected = vec![
                "int", "a", "=", "1", "+", "5", "+", "666", "+", "54", "+", "3", ";",
            ];

            test_valid(prog, &expected).await;
        }

        #[tokio::test]
        async fn test_unknown_replacement_token() {
            let prog = indoc! { r#"
                #define FOO 666 ` 54

                int a = 1 + 5 + FOO + 3;
            "# };

            test_invalid(prog, "Lex Error: Invalid Token ```").await;
        }

        #[tokio::test]
        async fn a_macro_body_with_an_escaped_quote_is_decoded_once_by_the_lexer() {
            let prog = indoc! { r#"
                #define GREETING "say \"hi\"\n"

                string s = GREETING;
            "# };

            let expected = vec!["string", "s", "=", "say \"hi\"\n", ";"];

            test_valid(prog, &expected).await;
        }
    }

    mod test_if {
        use super::*;

        #[tokio::test]
        async fn a_body_that_is_not_an_expression_substitutes_but_cannot_be_tested() {
            let mut preprocessor = fixture();
            preprocessor
                .scan("/define_text.c", "#define FOO 1 +\nint a = FOO 2;\n")
                .await
                .unwrap();

            let mut preprocessor = fixture();
            let e = preprocessor
                .scan("/define_text_if.c", "#define FOO 1 +\n#if FOO\n#endif\n")
                .await
                .unwrap_err();
            assert_eq!(
                e.to_string(),
                "`FOO` does not expand to a preprocessor expression"
            );
            assert_eq!(e.span().and_then(|s| s.code()).as_deref(), Some("#if FOO"));
        }

        #[tokio::test]
        async fn a_literal_one_takes_the_branch() {
            let prog = indoc! { r##"
                #if 1
                "taken";
                #endif
                #if 0
                "skipped";
                #endif
            "##};
            let expected = [
                Token::StringLiteral(StringToken(Span::new(0, 0..0), "taken".into())),
                Token::Semi(Span::new(0, 0..0)),
            ];
            let mut preprocessor = fixture();
            let tokens = preprocessor.scan("/if_literal.c", prog).await.unwrap();
            let kinds: Vec<_> = tokens.iter().map(std::mem::discriminant).collect();
            assert_eq!(
                kinds,
                expected
                    .iter()
                    .map(std::mem::discriminant)
                    .collect::<Vec<_>>()
            );
        }

        #[tokio::test]
        async fn a_malformed_if_is_invalid() {
            test_invalid("#iffy\n#endif\n", "unknown preprocessor directive `#iffy`").await;
        }

        #[tokio::test]
        async fn an_expression_error_points_at_the_offending_token() {
            let mut preprocessor = fixture();
            let e = preprocessor
                .scan("/if_lex_error.c", "int a;\n#if 1 + `\n#endif\n")
                .await
                .unwrap_err();
            assert_eq!(e.to_string(), "Lex Error: Invalid Token ```");
            assert_eq!(e.span().and_then(|s| s.code()).as_deref(), Some("`"));
        }

        #[tokio::test]
        async fn a_define_body_lex_error_carets_the_byte() {
            let mut preprocessor = fixture();
            let e = preprocessor
                .scan("/def_lex_error.c", "#define FOO 666 ` 54\n")
                .await
                .unwrap_err();
            assert_eq!(e.to_string(), "Lex Error: Invalid Token ```");
            assert_eq!(e.span().and_then(|s| s.code()).as_deref(), Some("`"));
        }

        #[tokio::test]
        async fn a_directive_in_a_macro_body_carets_the_directive() {
            let mut preprocessor = fixture();
            let e = preprocessor
                .scan("/def_directive.c", "#define FOO 1 + #undef BAR\n")
                .await
                .unwrap_err();
            assert_eq!(
                e.to_string(),
                "a preprocessor directive cannot appear in a macro body"
            );
            assert_eq!(
                e.span().and_then(|s| s.code()).as_deref(),
                Some("#undef BAR")
            );
        }

        #[tokio::test]
        async fn an_unknown_directive_is_named_live() {
            test_invalid("#elif 1\n", "unknown preprocessor directive `#elif`").await;
        }

        #[tokio::test]
        async fn defined_and_not_are_ordinary_identifiers() {
            // They used to lex as preprocessor keywords the main parser
            // rejects as "Unrecognized Token" (R7).
            let mut preprocessor = fixture();
            let tokens = preprocessor
                .scan("/unreserved.c", "int not = 1;\nint defined = 2;\n")
                .await
                .expect("scans clean");
            let ids: Vec<_> = tokens
                .iter()
                .filter_map(|t| match t {
                    Token::Id(s) => Some(s.1.as_str()),
                    _ => None,
                })
                .collect();
            assert_eq!(ids, vec!["not", "defined"]);
        }

        #[tokio::test]
        async fn defined_and_not_parse_as_ordinary_identifiers() {
            // The pin above checks scan-level token kinds; the spec's
            // promise is that the parser accepts them too.
            use crate::{compiler::lexer::TokenTriples, lpc_parser};

            let mut preprocessor = fixture();
            let tokens = preprocessor
                .scan("/unreserved.c", "int not = 1;\nint defined = 2;\n")
                .await
                .expect("scans clean");

            lpc_parser::ProgramParser::new()
                .parse(
                    &mut CompilationContext::default(),
                    TokenTriples::new(&tokens),
                )
                .expect("`not` and `defined` parse as plain identifiers");
        }

        #[tokio::test]
        async fn test_simple_if() {
            let prog = indoc! { r##"
                #define FOO 1
                #define BAZ 0
                #if FOO
                    "#if FOO works"
                #endif
                #if BAZ
                    "#if BAZ works, but should not"
                #endif
                #if QUUX
                    "#if QUUX works, but should not"
                #endif
            "## };

            test_valid(prog, &["#if FOO works"]).await
        }

        #[tokio::test]
        async fn test_simple_if_defined() {
            let prog = indoc! { r##"
                #define FOO 1
                #define BAR
                #define BAZ 0
                #if defined(FOO)
                    "#if defined(FOO) works"
                #endif
                #if defined (BAR)
                    "#if defined (BAR) works"
                #endif
                #if defined(BAZ)
                    "#if defined(BAZ) works"
                #endif
                #if defined(QUUX)
                    "#if QUUX works, but should not"
                #endif
            "## };

            test_valid(
                prog,
                &[
                    "#if defined(FOO) works",
                    "#if defined (BAR) works",
                    "#if defined(BAZ) works",
                ],
            )
            .await
        }

        #[tokio::test]
        async fn test_if_expressions() {
            let prog = indoc! { r##"
                #define FOO 1
                #define BAR
                #define BAZ 0

                #if defined(FOO) || defined (BAR)
                    "first test passes"
                #endif

                #if defined(BAR) || 1
                    "second test passes"
                #endif

                #if 1 || 0
                    "third test passes"
                #endif

                #if defined(QUUX) || BAZ
                    "this should not be printed"
                #endif

                #if defined(BAZ) && defined(FOO)
                    "fourth test passes"
                #endif

                #if FOO && defined (QUUX)
                    "this should not be printed"
                #endif

                #if defined(FOO) && defined(BAR)
                    "fifth test passes"
                #endif

                #if defined(FOO) && (defined(BAR) || defined(BAZ))
                    "sixth test passes"
                #endif

                #if not defined(FOO) || not defined(UNDEFINED)
                    "seventh test passes"
                #endif
            "## };

            test_valid(
                prog,
                &[
                    "first test passes",
                    "second test passes",
                    "third test passes",
                    "fourth test passes",
                    "fifth test passes",
                    "sixth test passes",
                    "seventh test passes",
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn test_macro_expansion() {
            let prog = indoc! { r##"
                #define FOO 1
                #define BAR (FOO - 1)
                #if BAR
                    "#if BAR works, but should not"
                #endif
            "## };

            test_valid(prog, &[]).await
        }

        #[tokio::test]
        async fn test_self_referential_if_is_false() {
            let prog = indoc! { r##"
                #define A A
                #if A
                "yes"
                #else
                "no"
                #endif
            "## };

            test_valid(prog, &["no"]).await;
        }

        #[tokio::test]
        async fn test_string_bodied_define_is_truthy() {
            let prog = indoc! { r##"
                #define A "str"
                #if A
                "yes"
                #else
                "no"
                #endif
            "## };

            test_valid(prog, &["yes"]).await;
        }

        #[tokio::test]
        async fn test_mutually_recursive_if_is_false() {
            let prog = indoc! { r##"
                #define A B
                #define B A
                #if A
                "yes"
                #else
                "no"
                #endif
            "## };

            test_valid(prog, &["no"]).await;
        }

        #[tokio::test]
        async fn test_self_referential_arithmetic_errors() {
            let prog = indoc! { r##"
                #define A A
                #if A + 0
                #endif
            "## };

            test_invalid(prog, "unable to resolve into an int").await;
        }

        #[tokio::test]
        async fn test_defined_is_an_integer() {
            let prog = indoc! { r##"
                #define A 1
                #define D 1
                #define E 2
                #if defined(A) + defined(B)
                "one"
                #endif
                #if defined(B) + defined(C)
                "zero"
                #else
                "none"
                #endif
                #if defined(D) + defined(E)
                "two"
                #endif
            "## };

            test_valid(prog, &["one", "none", "two"]).await;
        }

        #[tokio::test]
        async fn test_if_arithmetic_wraps() {
            let prog = indoc! { r##"
                #if 9223372036854775807 + 1
                "wrapped"
                #endif
            "## };

            test_valid(prog, &["wrapped"]).await;
        }

        #[tokio::test]
        async fn deep_acyclic_chain_in_if_nests_too_deeply() {
            // A chain of distinct names never trips the hide set (it only
            // catches cycles); without a depth bound this recurses one
            // Rust frame per hop and would eventually blow the stack.
            let mut prog = String::from("#define B0 1\n");
            for i in 1..=300 {
                prog.push_str(&format!("#define B{i} B{}\n", i - 1));
            }
            prog.push_str("#if B300\n#endif\n");

            test_invalid(&prog, "nests too deeply").await;
        }

        #[tokio::test]
        async fn deep_acyclic_chain_in_if_arithmetic_nests_too_deeply() {
            // Same chain, but forced through `resolve_int`'s `Var` arm via
            // an arithmetic position, pinning that arm's bound too.
            let mut prog = String::from("#define B0 1\n");
            for i in 1..=300 {
                prog.push_str(&format!("#define B{i} B{}\n", i - 1));
            }
            prog.push_str("#if B300 + 0\n#endif\n");

            test_invalid(&prog, "nests too deeply").await;
        }

        #[test]
        fn test_unknown_operation_is_an_error_not_a_panic() {
            let preprocessor = fixture();
            let node = PreprocessorNode::BinaryOp(
                BinaryOperation::Mul,
                Box::new(PreprocessorNode::Int(1)),
                Box::new(PreprocessorNode::Int(2)),
            );
            let e = preprocessor
                .eval_expr_for_skipping(&node, None, &mut Vec::new())
                .unwrap_err();
            assert_regex!(e.message(), "unknown binary operation");
        }
    }

    mod test_macros {
        use super::*;

        #[tokio::test]
        async fn test_functional_macros() {
            let prog = indoc! { r##"
                #define FOO 1234
                #define BAR(a, b) (a + b + FOO)
                #if BAR
                    "should not print. Functional macros themselves have a defined value of 0"
                #endif
                666 + BAR(5, 7)
            "## };

            test_valid(prog, &["666", "+", "(", "5", "+", "7", "+", "1234", ")"]).await
        }

        #[tokio::test]
        async fn arguments_expand_before_substitution() {
            let prog = indoc! { r##"
                #define TP this_player()
                #define ENV(x) environment(x)
                ENV(TP)
            "## };

            test_valid(prog, &["environment", "(", "this_player", "(", ")", ")"]).await
        }

        #[tokio::test]
        async fn test_uses_latest_value() {
            let prog = indoc! { r##"
                #define FOO 1234
                #define BAR FOO
                BAR
                #undef FOO
                #define FOO 4567
                BAR
            "## };

            test_valid(prog, &["1234", "4567"]).await
        }

        #[tokio::test]
        async fn test_bare_function_macro_name_is_an_identifier() {
            let prog = indoc! { r##"
                #define BAR(a, b) (a - b)
                BAR;
            "## };

            test_valid(prog, &["BAR", ";"]).await;
        }

        #[tokio::test]
        async fn test_errors_if_unterminated_call() {
            let prog = indoc! { r##"
                #define BAR(a, b) (a - b)
                BAR(dump("asdf");
            "## };

            test_invalid(prog, "unterminated call to macro `BAR`").await;
        }

        #[tokio::test]
        async fn test_errors_if_wrong_arg_count() {
            let prog = indoc! { r##"
                #define BAR(a, b) (a - b)
                BAR(34);
            "## };

            test_invalid(prog, "macro `BAR` takes 2 arguments, 1 given").await;
        }
    }

    mod test_pragmas {
        use super::*;

        #[tokio::test]
        async fn test_pragmas() {
            let prog = indoc! { r##"
                #pragma strict_types
                #pragma no_clone,resident ,  no_shadow
            "## };

            let mut preprocessor = fixture();
            match preprocessor.scan("test.c", prog).await {
                Ok(_) => {
                    assert!(preprocessor.context.pragmas.strict_types());
                    assert!(preprocessor.context.pragmas.no_clone());
                    assert!(preprocessor.context.pragmas.resident());
                    assert!(preprocessor.context.pragmas.no_shadow());

                    assert!(!preprocessor.context.pragmas.no_inherit());
                }
                Err(e) => {
                    panic!("{e:?}")
                }
            }
        }

        #[tokio::test]
        async fn a_bare_pragma_is_an_error() {
            test_invalid("#pragma\n", "expected a pragma name after `#pragma`").await;
        }

        #[tokio::test]
        async fn pragma_once_is_a_known_pragma() {
            test_valid("#pragma once\n1\n", &["1"]).await;
        }

        #[tokio::test]
        async fn pragma_names_process_in_order() {
            test_invalid(
                "#pragma once, not_a_pragma\n",
                "unknown pragma `not_a_pragma`",
            )
            .await;
        }
    }

    #[tokio::test]
    async fn a_bare_undef_is_an_error() {
        test_invalid("#undef\n", "expected an identifier after `#undef`").await;
    }

    #[tokio::test]
    async fn a_null_directive_is_a_no_op() {
        test_valid("#\n1;\n", &["1", ";"]).await;
    }

    #[tokio::test]
    async fn test_undef_in_dead_region_is_inert() {
        let prog = indoc! { r##"
            #define FOO 1
            #ifdef NOPE
            #undef FOO
            #endif
            FOO
        "## };

        test_valid(prog, &["1"]).await;
    }

    #[tokio::test]
    async fn test_pragma_in_dead_region_is_inert() {
        let mut preprocessor = fixture();
        let prog = indoc! { r##"
            #ifdef NOPE
            #pragma no_clone
            #endif
        "## };
        let _ = preprocessor.scan("/test.c", prog).await.unwrap();

        assert!(!preprocessor.context.pragmas.no_clone());
    }

    #[tokio::test]
    async fn test_dead_region_diagnoses_nothing() {
        // Arity error, unterminated call, and a lex error (`#elif` has no
        // token) — all inside a dead region.
        let prog = indoc! { r##"
            #define BAR(a, b) (a - b)
            #ifdef NOPE
            BAR(1);
            BAR(2
            #elif spurious
            #endif
            "ok"
        "## };

        test_valid(prog, &["ok"]).await;
    }

    #[tokio::test]
    async fn test_include_inside_taken_conditional() {
        let prog = indoc! { r##"
            #define YES
            #ifdef YES
            #include "include/balanced_conditional.h"
            #endif
            marf
        "## };

        test_valid(
            prog,
            &[
                "int",
                "from_else",
                "=",
                "2",
                ";",
                "int",
                "from_header",
                "=",
                "3",
                ";",
                "marf",
            ],
        )
        .await;
    }

    #[tokio::test]
    async fn test_unbalanced_include_errors_on_its_own_frame() {
        let prog = indoc! { r##"
            #define YES
            #ifdef YES
            #include "include/unbalanced_conditional.h"
            #endif
        "## };

        test_invalid(prog, "Found `#if` without a corresponding `#endif`").await;
    }

    #[tokio::test]
    async fn test_nested_conditional_inside_else_emits_one_branch() {
        let prog = indoc! { r##"
            #ifdef NOPE
            "a"
            #else
            #ifdef ALSO_NOPE
            "b"
            #endif
            "c"
            #endif
        "## };

        test_valid(prog, &["c"]).await;
    }

    #[tokio::test]
    async fn test_nested_else_inside_outer_else_is_valid() {
        let prog = indoc! { r##"
            #ifdef NOPE
            "a"
            #else
            #ifdef X
            "b"
            #else
            "c"
            #endif
            "d"
            #endif
        "## };

        test_valid(prog, &["c", "d"]).await;
    }

    #[tokio::test]
    async fn test_duplicate_else_across_nesting_errors() {
        let prog = indoc! { r##"
            #ifdef NOPE
            #else
            #ifdef X
            #endif
            #else
            #endif
        "## };

        test_invalid(prog, "duplicate `#else` found").await;
    }

    #[tokio::test]
    async fn test_dead_if_operand_never_evaluated() {
        let prog = indoc! { r##"
            #ifdef NOPE
            #if total ! garbage ?
            #endif
            #if UNDEFINED_NAME + 0
            #endif
            #endif
            "ok"
        "## };

        test_valid(prog, &["ok"]).await;
    }

    #[tokio::test]
    async fn a_dead_midline_hash_is_text() {
        // C99 6.10.1: a skipped group's non-directive lines are text. A
        // mid-line `#if` in a dead region used to error on placement;
        // now nothing mid-line is a directive when dead — and it pushes
        // no frame, so the single #endif below balances the outer #if.
        let prog = indoc! { r#"
            #if 0
            x; #if BROKEN
            #endif
            "after";
        "# };
        test_valid(prog, &["after", ";"]).await;
    }

    #[tokio::test]
    async fn a_dead_lex_error_does_not_launder_a_midline_directive() {
        // The backtick is a dropped lex error; the mid-line `#if` after
        // it is text (C99 6.10.1), not a frame — the single `#endif`
        // below must balance the outer `#if`.
        let prog = "#if 0\n` #if BROKEN\n#endif\n\"after\";\n";
        test_valid(prog, &["after", ";"]).await;
    }

    #[tokio::test]
    async fn a_comment_closing_on_the_directive_line_is_legal() {
        // The `*/ #define` shape: the gap back to the previous token
        // spans the comment's newline (spec R2) — matches C's
        // first-token-on-the-line reading.
        let prog = "int a;\n/* header\n comment */ #define FOO 1\nFOO;\n";
        test_valid(prog, &["int", "a", ";", "1", ";"]).await;
    }

    #[tokio::test]
    async fn a_string_ending_in_newline_does_not_launder_a_directive() {
        // The old Display-string check saw a trailing `\n` inside the
        // *decoded string value* and accepted this. Position does not.
        test_invalid(
            "string s = \"ab\\n\" #define FOO 1\n",
            "preprocessor directives must appear on their own line",
        )
        .await;
    }

    #[tokio::test]
    async fn a_dead_unknown_directive_is_inert() {
        let prog = indoc! { r#"
            #if 0
            #elif WHATEVER
            #error also fine here
            #endif
            "after";
        "# };
        test_valid(prog, &["after", ";"]).await;
    }

    #[tokio::test]
    async fn junk_macro_parameters_are_an_error() {
        test_invalid(
            "#define F(1, 2) x\n",
            "macro parameters must be identifiers",
        )
        .await;
        test_invalid("#define F(a, a) x\n", "duplicate macro parameter `a`").await;
    }

    #[tokio::test]
    async fn a_spaced_paren_defines_an_object_macro() {
        // C99's rule: `(` not flush against the name = object macro
        // whose body starts with the paren.
        test_valid("#define F (x)\nF;\n", &["(", "x", ")", ";"]).await;
    }

    #[tokio::test]
    async fn trailing_junk_after_an_if_expression_is_an_error() {
        test_invalid(
            "#if 1 2\n#endif\n",
            "unexpected tokens after `#if` expression",
        )
        .await;
    }

    #[tokio::test]
    async fn an_unterminated_include_path_is_an_error() {
        test_invalid("#include \"foo.h\n", "unterminated path in `#include`").await;
    }

    #[tokio::test]
    async fn an_unterminated_directive_comment_is_an_error() {
        test_invalid(
            "#ifdef FOO\n#endif /* open\n",
            "unterminated comment in a preprocessor directive",
        )
        .await;
    }

    #[tokio::test]
    async fn a_bare_if_is_an_error() {
        test_invalid("#if\n#endif\n", "expected an expression after `#if`").await;
    }

    #[tokio::test]
    async fn defined_without_a_call_shape_is_a_variable() {
        // `defined` alone is an (undefined) name: bare-name falsy (R5/R7).
        let prog = indoc! { r#"
            #if defined
            "should not appear"
            #endif
            "after";
        "# };
        test_valid(prog, &["after", ";"]).await;
    }

    #[tokio::test]
    async fn an_indented_directive_is_well_placed() {
        test_valid(
            "int x;\n    #define A 1\nA;\n",
            &["int", "x", ";", "1", ";"],
        )
        .await;
    }

    #[tokio::test]
    async fn a_multi_line_call_does_not_launder_a_directive() {
        // The consumed call's newline used to sit in the placement gap
        // (card ③'s documented micro-hole, closed by card ⑥ R4).
        test_invalid(
            "#define F(a, b) a + b\nint x = F(1,\n2) #define X 1\n",
            "preprocessor directives must appear on their own line",
        )
        .await;
    }

    #[tokio::test]
    async fn a_multi_line_string_argument_does_not_launder_a_directive() {
        test_invalid(
            "#define STR(s) s\nstring q = STR(\"a\nb\") #define X 1\n",
            "preprocessor directives must appear on their own line",
        )
        .await;
    }

    #[tokio::test]
    async fn a_zero_token_expansion_still_anchors_past_its_call() {
        // Nothing is emitted, so only the reported use span can anchor.
        test_invalid(
            "#define F(a, b)\nF(1,\n2) #define X 1\n",
            "preprocessor directives must appear on their own line",
        )
        .await;
    }

    #[tokio::test]
    async fn a_directive_after_a_multi_line_call_line_is_well_placed() {
        // The gap's own newline — after the `)` — still counts.
        test_valid(
            "#define F(a, b) a\nint x = F(1,\n2);\n#define X 1\nX;\n",
            &["int", "x", "=", "1", ";", "1", ";"],
        )
        .await;
    }

    mod test_include_walk {
        use super::*;

        fn temp_lib(name: &str) -> std::path::PathBuf {
            let root = std::env::temp_dir()
                .join(format!("lpc-rs-preprocessor-{name}-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&root);
            std::fs::create_dir_all(&root).unwrap();
            root
        }

        fn fixture_at(root: &std::path::Path) -> Preprocessor {
            let config = ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .build()
                .unwrap();
            let context = CompilationContextBuilder::default()
                .filename(Arc::new("main.c".into()))
                .config(config)
                .build()
                .unwrap();
            Preprocessor::new(context)
        }

        async fn tokens_of(root: &std::path::Path, code: &str) -> Vec<String> {
            let mut preprocessor = fixture_at(root);
            let scanned = preprocessor.scan("/main.c", code).await.unwrap();
            scanned.iter().map(|t| t.to_string()).collect()
        }

        async fn error_of(root: &std::path::Path, code: &str) -> LpcError {
            let mut preprocessor = fixture_at(root);
            preprocessor
                .scan("/main.c", code)
                .await
                .expect_err("expected the scan to fail")
        }

        #[tokio::test]
        async fn an_include_cycle_renders_the_chain() {
            let root = temp_lib("cycle-chain");
            std::fs::write(root.join("a.h"), "#include \"b.h\"\n").unwrap();
            std::fs::write(root.join("b.h"), "#include \"a.h\"\n").unwrap();

            let e = error_of(&root, "#include \"a.h\"\n").await;
            let rendered = e.diagnostic_string().replace(root.to_str().unwrap(), "");

            assert_eq!(
                rendered,
                "error: cyclic `#include`: `/a.h` is already being included\n  ┌─ /b.h:1:1\n  │\n1 │ #include \"a.h\"\n  │ ^^^^^^^^^^^^^^\n  │\n  ┌─ /main.c:1:1\n  │\n1 │ #include \"a.h\"\n  │ -------------- included from here\n  │\n  ┌─ /a.h:1:1\n  │\n1 │ #include \"b.h\"\n  │ -------------- included from here\n\n"
            );
        }

        #[tokio::test]
        async fn a_self_include_is_a_cycle() {
            let root = temp_lib("self-include");
            std::fs::write(root.join("a.h"), "#include \"a.h\"\n").unwrap();
            let e = error_of(&root, "#include \"a.h\"\n").await;
            assert_eq!(
                e.to_string(),
                "cyclic `#include`: `/a.h` is already being included"
            );
        }

        #[tokio::test]
        async fn a_header_including_the_root_is_a_cycle() {
            let root = temp_lib("root-cycle");
            std::fs::write(root.join("a.h"), "#include \"main.c\"\n").unwrap();
            let e = error_of(&root, "#include \"a.h\"\n1\n").await;
            assert_eq!(
                e.to_string(),
                "cyclic `#include`: `/main.c` is already being included"
            );
        }

        #[tokio::test]
        async fn a_once_root_reincluded_is_skipped_not_cyclic() {
            let root = temp_lib("once-root");
            std::fs::write(root.join("a.h"), "#include \"main.c\"\n1\n").unwrap();
            let tokens = tokens_of(&root, "#pragma once\n#include \"a.h\"\n2\n").await;
            assert_eq!(tokens, vec!["1", "2"]);
        }

        #[tokio::test]
        async fn a_chain_at_the_cap_scans() {
            let root = temp_lib("depth-at-cap");
            // Root + h0..h(leaf) fill the cap exactly.
            let leaf = include::MAX_INCLUDE_DEPTH - 2;
            for i in 0..leaf {
                std::fs::write(
                    root.join(format!("h{i}.h")),
                    format!("#include \"h{}.h\"\n", i + 1),
                )
                .unwrap();
            }
            std::fs::write(root.join(format!("h{leaf}.h")), "1\n").unwrap();
            let tokens = tokens_of(&root, "#include \"h0.h\"\n").await;
            assert_eq!(tokens, vec!["1"]);
        }

        #[tokio::test]
        async fn a_chain_past_the_cap_nests_too_deeply() {
            let root = temp_lib("depth-past-cap");
            let leaf = include::MAX_INCLUDE_DEPTH - 1;
            for i in 0..leaf {
                std::fs::write(
                    root.join(format!("h{i}.h")),
                    format!("#include \"h{}.h\"\n", i + 1),
                )
                .unwrap();
            }
            std::fs::write(root.join(format!("h{leaf}.h")), "1\n").unwrap();
            let e = error_of(&root, "#include \"h0.h\"\n").await;
            assert_eq!(e.to_string(), "`#include` nests too deeply");
        }

        #[tokio::test]
        async fn a_pragma_once_header_includes_once() {
            let root = temp_lib("once-header");
            std::fs::write(root.join("o.h"), "#pragma once\n1\n").unwrap();
            let tokens = tokens_of(&root, "#include \"o.h\"\n#include \"o.h\"\n").await;
            assert_eq!(tokens, vec!["1"]);
        }

        #[tokio::test]
        async fn a_dead_pragma_once_is_inert() {
            let root = temp_lib("dead-once");
            std::fs::write(root.join("d.h"), "#if 0\n#pragma once\n#endif\n1\n").unwrap();
            let tokens = tokens_of(&root, "#include \"d.h\"\n#include \"d.h\"\n").await;
            assert_eq!(tokens, vec!["1", "1"]);
        }
    }
}
