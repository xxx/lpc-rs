use std::{collections::HashMap, fmt::Debug, iter::Peekable, path::Path};

use async_recursion::async_recursion;
use define::{Define, ObjectMacro};
use lalrpop_util::ParseError as LalrpopParseError;
use lpc_rs_core::{
    convert_escapes,
    lpc_path::LpcPath,
    pragma_flags::{NO_CLONE, NO_INHERIT, NO_SHADOW, RESIDENT, STRICT_TYPES},
    LpcIntInner,
};
use lpc_rs_errors::{
    format_expected, lazy_files::FILE_CACHE, lpc_error, span::Span, LpcError, Result,
};
use lpc_rs_utils::read_lpc_file;
use once_cell::sync::Lazy;
use regex::Regex;
use tracing::{instrument, trace};

use crate::{
    compiler::{
        ast::binary_op_node::BinaryOperation,
        compilation_context::CompilationContext,
        lexer::{
            logos_token::{IntToken, StringToken},
            LexWrapper, Spanned, Token, TokenVecWrapper,
        },
        preprocessor::preprocessor_node::PreprocessorNode,
    },
    preprocessor_parser,
};

pub mod define;
pub mod preprocessor_node;

macro_rules! regex {
    ($re:literal $(,)?) => {{
        Lazy::new(|| Regex::new($re).unwrap())
    }};
}

static SYS_INCLUDE: Lazy<Regex> = regex!(r"\A\s*#\s*include\s+<([^>]+)>\s*\z");
static LOCAL_INCLUDE: Lazy<Regex> = regex!("\\A\\s*#\\s*include\\s+\"([^\"]+)\"[^\\S\n]*\n?\\z");
static DEFINE: Lazy<Regex> =
    regex!("\\A\\s*#\\s*define\\s+([\\p{Alphabetic}_]\\w*)(?:\\s*((?:\\\\.|[^\n])*))?\n?\\z");
static DEFINEMACRO: Lazy<Regex> = regex!(
    "\\A\\s*#\\s*define\\s+([\\p{Alphabetic}_]\\w*)\\(([^)]*)\\)\\s*((?:\\\\.|[^\n])*)\n?\\z"
);
static UNDEF: Lazy<Regex> = regex!(r#"\A\s*#\s*undef\s+([\p{Alphabetic}_]\w*)\s*\z"#);
static IF: Lazy<Regex> = regex!("\\A\\s*#\\s*if\\s+([^\n]*)\\s*\\z");
static IFDEF: Lazy<Regex> = regex!(r#"\A\s*#\s*ifdef\s+([\p{Alphabetic}_]\w*)\s*\z"#);
static IFNDEF: Lazy<Regex> = regex!(r#"\A\s*#\s*ifndef\s+([\p{Alphabetic}_]\w*)\s*\z"#);
// static ENDIF: Lazy<Regex> = regex!(r#"\A\s*#\s*endif\s*\z"#);
static ELSE: Lazy<Regex> = regex!(r#"\A\s*#\s*else\s*\z"#);
static PRAGMA: Lazy<Regex> =
    regex!(r#"\A\s*#\s*pragma\s+([\p{Alphabetic}_]\w*(?:\s*,\s*[\p{Alphabetic}_]\w*)*)\s*\z"#);
static COMMA_SEPARATOR: Lazy<Regex> = regex!(r"\s*,\s*");

#[derive(Debug)]
struct IfDef {
    pub span: Span,

    /// This field on the top IfDef in the stack indicates if we're currently in
    /// a section that's conditionally compiled out. We mutate this field
    /// when we see #else.
    pub skipping_lines: bool,

    /// Is this `#ifdef` itself conditionally compiled out?
    pub compiled_out: bool,
}

#[derive(Debug)]
pub struct Preprocessor {
    /// The compilation context
    context: CompilationContext,

    /// We keep track of `#define`d things here.
    defines: HashMap<String, Define>,

    /// Stack of ifdefs that are in play, so we can handle `#else` and `#endif`s
    ifdefs: Vec<IfDef>,

    /// Have we seen an `#else` clause for the current `#if`?
    current_else: Option<Span>,

    /// We Track the last slice, because things like preprocessor directives
    /// need to check it.
    last_slice: String,
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
    pub async fn scan<P, C>(&mut self, path: P, code: C) -> Result<Vec<Spanned<Token>>>
    where
        P: Into<LpcPath>,
        C: AsRef<str> + Send,
    {
        let mut output = vec![];

        let lpc_path = path.into();

        trace!("scanning {:?} :: {:?}", lpc_path, code.as_ref());

        // handle auto-include
        if let Some(auto_include) = &self.context.config.auto_include_file {
            let auto_include_path =
                LpcPath::new_server(format!("{}/{}", &self.context.config.lib_dir, auto_include));

            if auto_include_path != lpc_path {
                let included = self.include_local_file(&auto_include_path, None).await?;

                for spanned in included {
                    self.append_spanned(&mut output, spanned)
                }
            }
        }

        self.internal_scan(&lpc_path, code, Some(output)).await
    }

    /// The recursive function that takes care of scanning everything.
    #[async_recursion]
    async fn internal_scan<C>(
        &mut self,
        lpc_path: &LpcPath,
        code: C,
        existing_output: Option<Vec<Spanned<Token>>>,
    ) -> Result<Vec<Spanned<Token>>>
    where
        C: AsRef<str> + Send,
    {
        let mut output = existing_output.unwrap_or_default();

        let file_id = {
            let server_path = lpc_path.as_server(self.context.config.lib_dir.as_str());
            let mut cache = FILE_CACHE.write();
            if Path::exists(&server_path) {
                cache.add(server_path.to_string_lossy())
            } else {
                // We pretend the eager file has an absolute path in the cache, as it makes lookups easier.
                cache.add_eager(server_path.to_string_lossy(), code.as_ref())
                // cache.add_eager(lpc_path.to_string_lossy(), code.as_ref())
            }
        };

        let mut token_stream = LexWrapper::new(code.as_ref());
        token_stream.set_file_id(file_id);

        let mut iter = token_stream.peekable();

        while let Some(spanned_result) = iter.next() {
            match spanned_result {
                Ok(spanned) => {
                    let (_l, token, _r) = &spanned;

                    let token_string = token.to_string();

                    match token {
                        Token::LocalInclude(t) => {
                            let cwd = lpc_path
                                .as_in_game(self.context.config.lib_dir.as_str())
                                .parent()
                                .unwrap_or_else(|| Path::new("/"))
                                .to_path_buf();
                            self.handle_local_include(t, &cwd, &mut output).await?
                        }
                        Token::SysInclude(t) => {
                            let cwd = lpc_path
                                .as_in_game(self.context.config.lib_dir.as_str())
                                .parent()
                                .unwrap_or_else(|| Path::new("/"))
                                .to_path_buf();
                            self.handle_sys_include(t, &cwd, &mut output).await?
                        }
                        Token::PreprocessorElse(t) => self.handle_else(t)?,
                        Token::Endif(t) => self.handle_endif(t)?,
                        Token::Define(t) => self.handle_define(t)?,
                        Token::Undef(t) => self.handle_undef(t)?,
                        Token::PreprocessorIf(t) => self.handle_if(t)?,
                        Token::IfDef(t) => self.handle_ifdef(t)?,
                        Token::IfNDef(t) => self.handle_ifndef(t)?,
                        Token::Pragma(t) => self.handle_pragma(t)?,

                        Token::NewLine(_) => { /* Ignore */ }

                        // Handle macro expansion
                        Token::Id(t) => {
                            let appends = self.expand_token(t, &mut iter)?;

                            match appends {
                                Some(mut vec) => {
                                    if !self.skipping_lines() {
                                        output.append(&mut vec)
                                    }
                                }
                                None => self.append_spanned(&mut output, spanned),
                            }
                        }
                        _ => self.append_spanned(&mut output, spanned),
                    }

                    self.last_slice = token_string;
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        if let Some(ifdef) = self.ifdefs.last() {
            return Err(lpc_error!(
                Some(ifdef.span),
                "Found `#if` without a corresponding `#endif`"
            ));
        }

        Ok(output)
    }

    /// Expand a `#define`d token, if necessary
    #[instrument(skip(self))]
    fn expand_token<T>(
        &self,
        token: &StringToken,
        iter: &mut Peekable<T>,
    ) -> Result<Option<Vec<Spanned<Token>>>>
    where
        T: Iterator<Item = Result<Spanned<Token>>> + Debug,
    {
        let span = token.0;
        let name = &token.1;

        match self.defines.get(name) {
            Some(Define::Object(object)) => {
                let mut tokens = Vec::with_capacity(object.tokens.len());

                for (tl, tok, tr) in object.tokens.clone() {
                    match &tok {
                        Token::Id(s) => {
                            let mut iter = TokenVecWrapper::new(&object.tokens).peekable();

                            match self.expand_token(s, &mut iter)? {
                                Some(mut vec) => tokens.append(&mut vec),
                                None => tokens.push((tl, Token::Id(s.clone()), tr)),
                            }
                        }
                        _ => {
                            // Set the span to that of the token before its replacement.
                            // let new_spanned = (span.l, tok.with_span(span), span.r);
                            // tokens.push(new_spanned);
                            tokens.push((span.l, tok, span.r));
                        }
                    }
                }
                Ok(Some(tokens))
            }
            Some(Define::Function(function)) => {
                if !matches!(iter.peek(), Some(Ok((_, Token::LParen(_), _)))) {
                    return Err(lpc_error!(
                        Some(token.0),
                        "functional macro call missing arguments"
                    ));
                }

                let args = Preprocessor::consume_macro_arguments(iter, span)?;

                if args.len() != function.args.len() {
                    return Err(lpc_error!(
                        Some(span),
                        "incorrect number of macro arguments"
                    ));
                }

                let arg_map = function
                    .args
                    .iter()
                    .cloned()
                    .zip(args)
                    .collect::<HashMap<_, _>>();

                let mut replacements = Vec::with_capacity(function.tokens.len());
                let mut iter = TokenVecWrapper::new(&function.tokens).peekable();

                while let Some(Ok(replacement)) = iter.next() {
                    if let (tl, Token::Id(s), tr) = replacement {
                        if let Some(arg_tokens) = arg_map.get(&s.1) {
                            replacements.append(&mut arg_tokens.clone());
                        } else {
                            match self.expand_token(&s, &mut iter)? {
                                Some(mut vec) => replacements.append(&mut vec),
                                None => replacements.push((tl, Token::Id(s.clone()), tr)),
                            }
                        }
                    } else {
                        replacements.push(replacement.clone())
                    }
                }

                Ok(Some(replacements))
            }
            None => Ok(None),
        }
    }

    /// Consume tokens until the end of the arguments list, then collect them
    /// into a vector-per-argument.
    /// Assumes the next token in the iterator is the opening left parenthesis,
    /// and has already been checked for its presence.
    #[instrument]
    fn consume_macro_arguments<T>(
        iter: &mut Peekable<T>,
        span: Span,
    ) -> Result<Vec<Vec<Spanned<Token>>>>
    where
        T: Iterator<Item = Result<Spanned<Token>>> + Debug,
    {
        iter.next(); // consume the opening paren

        let mut parens = 1;
        let mut args: Vec<Vec<Spanned<Token>>> = vec![];
        let mut arg = vec![];

        while parens != 0 {
            let next = iter.next();

            match next {
                Some(Ok(t)) => {
                    let (_, arg_tok, _) = &t;
                    match &arg_tok {
                        Token::LParen(_) => {
                            parens += 1;
                            arg.push(t);
                        }
                        Token::RParen(_) => {
                            parens -= 1;

                            if parens == 0 {
                                args.push(arg);
                                arg = vec![];
                            } else {
                                arg.push(t);
                            }
                        }
                        Token::Comma(_) => {
                            if parens == 1 {
                                // we're inside only the outermost parens
                                args.push(arg);
                                arg = vec![];
                            } else {
                                arg.push(t)
                            }
                        }
                        Token::NewLine(_) => { /* ignore */ }
                        _ => {
                            arg.push(t);
                        }
                    }
                }
                Some(Err(e)) => return Err(e),
                None => break,
            }
        }

        if parens != 0 {
            return Err(lpc_error!(Some(span), "mismatched parentheses"));
        }

        Ok(args)
    }

    #[instrument(skip(self))]
    fn handle_define(&mut self, token: &StringToken) -> Result<()> {
        if self.skipping_lines() {
            return Ok(());
        }

        let span = token.0;

        self.check_for_previous_newline(span)?;

        let check_duplicate = |key, error_span| {
            if !self.skipping_lines() && self.defines.contains_key(key) {
                return Err(LpcError::new(format!("duplicate `#define`: `{key}`"))
                    .with_span(Some(error_span)));
            }

            Ok(())
        };

        let lex_vec = |input| -> Result<Vec<Spanned<Token>>> {
            let lexer = LexWrapper::new(input);

            lexer.collect()
        };

        if let Some(captures) = DEFINEMACRO.captures(&token.1) {
            check_duplicate(&captures[1], span)?;

            let name = String::from(&captures[1]);
            let args: Vec<String> = COMMA_SEPARATOR
                .split(&captures[2])
                .map(String::from)
                .collect();
            let body = &captures[3];

            // re-span these tokens to just be the entire #define line
            let tokens = lex_vec(&convert_escapes(body))?
                .into_iter()
                .map(|(_, t, _)| (span.l, t.with_span(span), span.r))
                .collect::<Vec<_>>();

            let define = Define::new_function(tokens, args);

            self.defines.insert(name, define);

            Ok(())
        } else if let Some(captures) = DEFINE.captures(&token.1) {
            check_duplicate(&captures[1], token.0)?;

            let name = String::from(&captures[1]);
            let tokens = if captures[2].is_empty() {
                vec![(span.l, Token::IntLiteral(IntToken(span, 0)), span.r)]
            } else {
                // tokenize captures[2] with our full language lexer, so we can store it
                lex_vec(&convert_escapes(&captures[2]))?
                    .into_iter()
                    .map(|(_, t, _)| (span.l, t.with_span(span), span.r))
                    .collect::<Vec<_>>()
            };

            let expr = if captures[2].is_empty() {
                PreprocessorNode::Int(0)
            } else {
                match preprocessor_parser::ExpressionParser::new()
                    .parse(LexWrapper::new(&captures[2]))
                {
                    Ok(i) => i,
                    Err(e) => {
                        return Err(lpc_error!(
                            Some(token.0),
                            "parse error: {}, for expression `{}`",
                            e,
                            &captures[2]
                        ))
                    }
                }
            };

            let define = Define::new_object(tokens, expr);

            self.defines.insert(name, define);
            Ok(())
        } else {
            Err(lpc_error!(Some(token.0), "invalid `#define`."))
        }
    }

    #[instrument(skip(self))]
    fn handle_undef(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = UNDEF.captures(&token.1) {
            self.defines.remove(&captures[1]);
        }

        Ok(())
    }

    /// # Arguments
    /// `token` - The matched lexer token
    /// `cwd` - an in-game directory, to use as the reference for relative paths.
    /// `output` - The vector to append included tokens to
    #[instrument(skip(self, output))]
    async fn handle_sys_include<U>(
        &mut self,
        token: &StringToken,
        cwd: &U,
        output: &mut Vec<Spanned<Token>>,
    ) -> Result<()>
    where
        U: AsRef<Path> + Debug,
    {
        if self.skipping_lines() {
            return Ok(());
        }

        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = SYS_INCLUDE.captures(&token.1) {
            let matched = captures.get(1).unwrap();

            let config = self.context.config.clone();
            for dir in &config.system_include_dirs {
                let to_include =
                    LpcPath::new_in_game(matched.as_str(), dir.as_str(), &*config.lib_dir);
                return match self.include_local_file(&to_include, Some(token.0)).await {
                    Ok(included) => {
                        for spanned in included {
                            self.append_spanned(output, spanned)
                        }

                        Ok(())
                    }
                    Err(e) => {
                        // TODO: bleeeeeeech. Errors should have a better way to handle this.
                        // If the error is just "file not found", keep looking
                        if (*e).as_ref().contains("unable to read include file") {
                            continue;
                        }

                        Err(e)
                    }
                };
            }

            let to_include = LpcPath::new_in_game(matched.as_str(), cwd, &*config.lib_dir);

            // Fall back to trying the path directly
            let included = self.include_local_file(&to_include, Some(token.0)).await?;

            for spanned in included {
                self.append_spanned(output, spanned)
            }

            Ok(())
        } else {
            Err(lpc_error!(Some(token.0), "invalid `#include`."))
        }
    }

    /// # Arguments
    /// `token` - The matched lexer token
    /// `cwd` - an in-game directory, to use as the reference for relative
    /// paths. `output` - The vector to append included tokens to
    #[instrument(skip(self, output))]
    #[async_recursion]
    async fn handle_local_include<U>(
        &mut self,
        token: &StringToken,
        cwd: &U,
        output: &mut Vec<Spanned<Token>>,
    ) -> Result<()>
    where
        U: AsRef<Path> + Debug + Send + Sync,
    {
        if self.skipping_lines() {
            return Ok(());
        }

        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = LOCAL_INCLUDE.captures(&token.1) {
            let matched = captures.get(1).unwrap();
            let to_include =
                LpcPath::new_in_game(matched.as_str(), cwd, &*self.context.config.lib_dir);

            let included = self.include_local_file(&to_include, Some(token.0)).await?;

            for spanned in included {
                self.append_spanned(output, spanned)
            }

            Ok(())
        } else {
            Err(lpc_error!(Some(token.0), "invalid `#include`."))
        }
    }

    #[instrument(skip(self))]
    fn handle_if(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = IF.captures(&token.1) {
            // parse the captures into an expression, then evaluate it.
            match preprocessor_parser::ExpressionParser::new().parse(LexWrapper::new(&captures[1]))
            {
                Ok(expr) => {
                    let printing_lines = self.eval_expr_for_skipping(&expr, Some(token.0))?;

                    self.ifdefs.push(IfDef {
                        // code: String::from(&captures[1]),
                        skipping_lines: !printing_lines,
                        compiled_out: self.skipping_lines(),
                        span: token.0,
                    });
                }
                Err(e) => {
                    // This is awkward due to being almost identical to the From<ParseError> impl,
                    // but we need to use our `token` parameter's span for the errors, rather than
                    // pulling it from the error's token.
                    // Is there a better way?
                    let err = match e {
                        LalrpopParseError::InvalidToken { location } => {
                            LpcError::new(format!("invalid token `{}` at {}", token.1, location))
                        }
                        LalrpopParseError::UnrecognizedEof { expected, .. } => {
                            LpcError::new("unexpected EOF").with_note(format_expected(&expected))
                        }
                        LalrpopParseError::UnrecognizedToken { expected, .. } => {
                            LpcError::new(format!("unrecognized Token: {}", token.1))
                                .with_span(Some(token.0))
                                .with_note(format_expected(&expected))
                        }
                        LalrpopParseError::ExtraToken { .. } => {
                            LpcError::new(format!("extra Token: `{}`", token.1))
                                .with_span(Some(token.0))
                        }
                        LalrpopParseError::User { error } => {
                            LpcError::new(format!("error: {error}"))
                        }
                    };

                    return Err(err.into());
                }
            }

            Ok(())
        } else {
            Err(lpc_error!(Some(token.0), "invalid `#ifdef`."))
        }
    }

    /// Determine if a particular node will enable line skipping or not.
    /// Returns `true` if we should print lines, and `false` if they should be
    /// skipped.
    #[instrument(skip(self, expr))]
    fn eval_expr_for_skipping(&self, expr: &PreprocessorNode, span: Option<Span>) -> Result<bool> {
        match expr {
            PreprocessorNode::Var(x) => {
                if let Some(Define::Object(ObjectMacro { expr, .. })) = self.defines.get(x) {
                    let int_val = self.resolve_int(expr, span)?;
                    Ok(int_val != 0)
                } else {
                    Ok(false)
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
                BinaryOperation::Add => {
                    Ok(self.resolve_int(l, span)? + self.resolve_int(r, span)? != 0)
                }
                BinaryOperation::Sub => {
                    Ok(self.resolve_int(l, span)? - self.resolve_int(r, span)? != 0)
                }
                BinaryOperation::AndAnd => Ok(self.eval_expr_for_skipping(l, span)?
                    && self.eval_expr_for_skipping(r, span)?),
                BinaryOperation::OrOr => Ok(self.eval_expr_for_skipping(l, span)?
                    || self.eval_expr_for_skipping(r, span)?),
                _ => unimplemented!(),
            },
        }
    }

    /// Resolve a [`PreprocessorNode`] to an Int if possible.
    #[instrument(skip(self, expr))]
    fn resolve_int(&self, expr: &PreprocessorNode, span: Option<Span>) -> Result<LpcIntInner> {
        match expr {
            PreprocessorNode::Var(x) => {
                if let Some(val) = self.defines.get(x) {
                    match val {
                        Define::Object(ObjectMacro { expr, .. }) => self.resolve_int(expr, span),
                        Define::Function(_) => Ok(0),
                    }
                } else {
                    Err(lpc_error!(span, "unable to resolve into an int: `{}`", x))
                }
            }
            PreprocessorNode::Int(i) => Ok(*i),
            PreprocessorNode::BinaryOp(op, l, r) => {
                let li = self.resolve_int(l, span)?;
                let ri = self.resolve_int(r, span)?;

                match op {
                    BinaryOperation::Add => Ok(li + ri),
                    BinaryOperation::Sub => Ok(li - ri),
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

    #[instrument(skip(self))]
    fn handle_ifdef(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        IFDEF.captures(&token.1).map_or_else(
            || Err(lpc_error!(Some(token.0), "invalid `#ifdef`.")),
            |captures| {
                self.ifdefs.push(IfDef {
                    // code: String::from(&captures[1]),
                    skipping_lines: !self.defines.contains_key(&captures[1]),
                    compiled_out: self.skipping_lines(),
                    span: token.0,
                });

                Ok(())
            },
        )
    }

    #[instrument(skip(self))]
    fn handle_ifndef(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        IFNDEF.captures(&token.1).map_or_else(
            || Err(lpc_error!(Some(token.0), "invalid `#ifndef`.")),
            |captures| {
                self.ifdefs.push(IfDef {
                    // code: String::from(&captures[1]),
                    skipping_lines: self.defines.contains_key(&captures[1]),
                    compiled_out: self.skipping_lines(),
                    span: token.0,
                });

                Ok(())
            },
        )
    }

    #[instrument(skip(self))]
    fn handle_else(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if ELSE.is_match(&token.1) {
            if self.ifdefs.is_empty() {
                return Err(lpc_error!(
                    Some(token.0),
                    "found `#else` without a corresponding `#if` or `#ifdef`",
                ));
            }

            if let Some(else_span) = &self.current_else {
                let err = LpcError::new("duplicate `#else` found")
                    .with_span(Some(token.0))
                    .with_label("first used here", Some(*else_span));

                return Err(err.into());
            }

            self.current_else = Some(token.0);

            if !self.current_if_is_compiled_out() {
                let last = self.ifdefs.last_mut().unwrap();
                last.skipping_lines = !last.skipping_lines;
            }

            Ok(())
        } else {
            Err(lpc_error!(Some(token.0), "invalid `#else`."))
        }
    }

    #[instrument(skip(self))]
    fn handle_endif(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if self.ifdefs.is_empty() {
            return Err(lpc_error!(
                Some(token.0),
                "found `#endif` without a corresponding `#if`"
            ));
        }

        self.ifdefs.pop();
        self.current_else = None;

        Ok(())
    }

    #[instrument(skip(self))]
    fn handle_pragma(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = PRAGMA.captures(&token.1) {
            for arg in COMMA_SEPARATOR.split(&captures[1]) {
                match arg {
                    NO_CLONE => self.context.pragmas.set_no_clone(true),
                    NO_INHERIT => self.context.pragmas.set_no_inherit(true),
                    NO_SHADOW => self.context.pragmas.set_no_shadow(true),
                    RESIDENT => self.context.pragmas.set_resident(true),
                    STRICT_TYPES => self.context.pragmas.set_strict_types(true),
                    x => {
                        return Err(lpc_error!(Some(token.0), "unknown pragma `{}`", x));
                    }
                }
            }
        }

        Ok(())
    }

    /// Read in a local file, and scan it through this preprocessor.
    ///
    /// # Arguments
    /// `path` - The path of the file we're going to scan. This is intended to
    /// be the file from     the `#include` directive.
    /// `cwd` - The current working directory. Used for resolving relative
    /// pathnames. `span` - The [`Span`] of the `#include` token.
    #[instrument(skip(self))]
    #[async_recursion]
    async fn include_local_file(
        &mut self,
        path: &LpcPath,
        span: Option<Span>,
    ) -> Result<Vec<Spanned<Token>>> {
        let canon_include_path = path.as_server(self.context.lib_dir());

        if !path.is_within_root(self.context.lib_dir()) {
            return Err(lpc_error!(
                span,
                "attempt to include a file outside the root: `{}` (expanded to `{}`) (lib_dir: `{}`)",
                path,
                canon_include_path.display(),
                self.context.lib_dir()
            ));
        }

        if canon_include_path.is_dir() {
            return Err(lpc_error!(
                span,
                "attempt to include a directory: `{}` (expanded to `{}`) (lib_dir: `{}`)",
                path,
                canon_include_path.display(),
                self.context.lib_dir()
            ));
        }

        let file_content = match read_lpc_file(&canon_include_path).await {
            Ok(content) => content,
            Err(e) => {
                return Err(lpc_error!(
                    span,
                    "unable to read include file `{}`: {:?}",
                    path,
                    e
                ));
            }
        };

        let path = LpcPath::new_server(canon_include_path);
        self.internal_scan(&path, &file_content, None).await
    }

    /// Are we skipping lines right now due to `#if`s?
    #[inline]
    #[instrument(skip(self))]
    fn skipping_lines(&self) -> bool {
        match self.ifdefs.last() {
            Some(ifdef) => ifdef.skipping_lines || ifdef.compiled_out,
            None => false,
        }
    }

    /// Is the current `#if` / `#ifdef` entirely compiled out?
    #[inline]
    #[instrument(skip(self))]
    fn current_if_is_compiled_out(&self) -> bool {
        match self.ifdefs.last() {
            Some(ifdef) => ifdef.compiled_out,
            None => false,
        }
    }

    /// skip-aware way to append to the output
    #[inline]
    #[instrument(skip(self, output, to_append))]
    fn append_spanned(&self, output: &mut Vec<Spanned<Token>>, to_append: Spanned<Token>) {
        if !self.skipping_lines() {
            output.push(to_append);
        }
    }

    /// A convenience function for checking if preprocessor directives follow a
    /// newline.
    #[instrument(skip(self))]
    fn check_for_previous_newline(&self, span: Span) -> Result<()> {
        if !self.last_slice.ends_with('\n') {
            return Err(lpc_error!(
                Some(span),
                "preprocessor directives must appear on their own line.",
            ));
        }

        Ok(())
    }
}

impl Default for Preprocessor {
    #[instrument]
    fn default() -> Self {
        Self {
            context: CompilationContext::default(),
            defines: HashMap::new(),
            ifdefs: vec![],
            current_else: None,
            last_slice: String::from("\n"),
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
                let mapped = result.iter().map(|i| i.1.to_string()).collect::<Vec<_>>();

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
                assert_regex!((*e).as_ref(), expected);
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

            test_invalid(input, "unable to read include file `/nonexistent.h`").await;
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

            test_invalid(prog, "invalid `#include`").await;
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

            test_invalid(input, "unable to read include file `/askdf/foo.h`").await;
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

            test_invalid(prog, "invalid `#include`").await;
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
                            expr: PreprocessorNode::Int(1234),
                            ..
                        })
                    ));
                    assert!(matches!(
                        preprocessor.defines.get("MAR").unwrap(),
                        Define::Object(ObjectMacro {
                            expr: PreprocessorNode::Int(0),
                            ..
                        })
                    ));
                    if let Define::Object(ObjectMacro { expr, .. }) =
                        preprocessor.defines.get("DOOD").unwrap()
                    {
                        assert_eq!(
                            expr,
                            &PreprocessorNode::BinaryOp(
                                BinaryOperation::Add,
                                Box::new(PreprocessorNode::Int(666)),
                                Box::new(PreprocessorNode::Var(String::from("MAR")))
                            )
                        );
                    } else {
                        panic!("Failed to match.")
                    }
                    assert_matches!(
                        preprocessor.defines.get("SNUH").unwrap(),
                        Define::Object(ObjectMacro {
                            expr: PreprocessorNode::Int(291),
                            ..
                        })
                    );
                    assert_matches!(
                        preprocessor.defines.get("TO").unwrap(),
                        Define::Object(ObjectMacro {
                            expr: PreprocessorNode::Int(1),
                            ..
                        })
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
                            expr: PreprocessorNode::Int(456),
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
                            expr: PreprocessorNode::Int(123),
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
        async fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #define
            "#
            };

            test_invalid(prog, "invalid `#define`").await;
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

            test_invalid(prog, "invalid `#ifdef`").await;
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

            test_invalid(prog, "invalid `#ifndef`").await;
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

            test_invalid(prog, "invalid `#else`").await;
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
    }

    mod test_if {
        use super::*;

        #[tokio::test]
        async fn test_simple_if() {
            let prog = indoc! { r##"
                #define FOO 1
                #define BAR
                #define BAZ 0
                #if FOO
                    "#if FOO works"
                #endif
                #if BAR
                    "#if BAR works, but should not"
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

                #if defined(FOO) && BAR
                    "this should not be printed"
                #endif

                #if defined(FOO) && (BAR || defined(BAZ))
                    "fifth test passes"
                #endif

                #if not defined(FOO) || not defined(UNDEFINED)
                    "sixth test passes"
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
        async fn test_errors_if_no_args() {
            let prog = indoc! { r##"
                #define BAR(a, b) (a - b)
                BAR;
            "## };

            test_invalid(prog, "functional macro call missing arguments").await;
        }

        #[tokio::test]
        async fn test_errors_if_mismatched_parens() {
            let prog = indoc! { r##"
                #define BAR(a, b) (a - b)
                BAR(dump("asdf");
            "## };

            test_invalid(prog, "mismatched parentheses").await;
        }

        #[tokio::test]
        async fn test_errors_if_wrong_arg_count() {
            let prog = indoc! { r##"
                #define BAR(a, b) (a - b)
                BAR(34);
            "## };

            test_invalid(prog, "incorrect number of macro arguments").await;
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
    }
}
