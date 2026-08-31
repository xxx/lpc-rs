//! The expansion engine: the one rewriter of token streams against the
//! define table. One cursor per stream, non-reexpansion via the hide set,
//! an output cap, paren-balanced argument capture.
//! Spec: local/specs/2026-08-30-expansion-engine.md.

use std::{collections::HashMap, iter::Peekable};

use lpc_rs_errors::{Result, lpc_error, span::Span};

use crate::compiler::{
    lexer::{Spanned, Token, TokenVecWrapper, logos_token::StringToken},
    preprocessor::define::{Define, FunctionMacro},
};

/// Hard cap on tokens one top-level expansion may emit (R5): the hide set
/// bounds depth, this bounds width.
const MAX_EXPANDED_TOKENS: usize = 65_536;

/// The engine over one define table. Cheap to build; one per use is fine.
pub(super) struct Expander<'a> {
    defines: &'a HashMap<String, Define>,
}

impl<'a> Expander<'a> {
    /// Build an engine over `defines`.
    pub(super) fn new(defines: &'a HashMap<String, Define>) -> Self {
        Self { defines }
    }

    /// Expand one potential macro use. `cursor` is the stream the use sits
    /// in; a function-macro's arguments are captured from it. `None`: not a
    /// macro use (undefined name, or a function-macro name with no `(`
    /// following — R2's plain-identifier rule); the caller emits the Id.
    pub(super) fn expand_use<T>(
        &self,
        token: &StringToken,
        cursor: &mut Peekable<T>,
    ) -> Result<Option<Vec<Spanned<Token>>>>
    where
        T: Iterator<Item = Result<Spanned<Token>>>,
    {
        let Some((name, define)) = self.defines.get_key_value(&token.1) else {
            return Ok(None);
        };
        if matches!(define, Define::Function(_))
            && !matches!(cursor.peek(), Some(Ok((_, Token::LParen(_), _))))
        {
            return Ok(None);
        }

        let mut expansion = Expansion {
            defines: self.defines,
            top: name,
            use_span: token.0,
            hide: vec![],
            emitted: 0,
        };
        let mut out = vec![];
        expansion.expand_named(name, cursor, &mut out)?;
        Ok(Some(out))
    }
}

/// One top-level use being expanded: the budget, the hide set, the use site.
struct Expansion<'a> {
    defines: &'a HashMap<String, Define>,
    /// The top-level macro, named by the cap diagnostic.
    top: &'a str,
    /// Every body-derived token emits with this span (R10).
    use_span: Span,
    /// Names being expanded on the current path (R4). A stack; `contains`
    /// is the membership test — macro chains are short.
    hide: Vec<&'a str>,
    emitted: usize,
}

/// No parameters to substitute: object-macro bodies and argument streams.
fn no_args() -> HashMap<&'static str, Vec<Spanned<Token>>> {
    HashMap::new()
}

impl<'a> Expansion<'a> {
    /// Expand the already-looked-up macro `name`, arguments from `cursor`.
    fn expand_named<T>(
        &mut self,
        name: &'a str,
        cursor: &mut Peekable<T>,
        out: &mut Vec<Spanned<Token>>,
    ) -> Result<()>
    where
        T: Iterator<Item = Result<Spanned<Token>>>,
    {
        match self.defines.get(name).expect("caller looked the name up") {
            Define::Object(object) => {
                self.hide.push(name);
                let walked = self.walk(&object.tokens, &no_args(), true, out);
                self.hide.pop();
                walked
            }
            Define::Function(function) => {
                cursor.next(); // the `(` the caller peeked
                let raw = self.capture_arguments(name, cursor)?;
                let args = self.bind_arguments(name, function, raw)?;
                self.hide.push(name);
                let walked = self.walk(&function.tokens, &args, true, out);
                self.hide.pop();
                walked
            }
        }
    }

    /// Walk one stream with a single cursor (R2), substituting parameters
    /// and expanding nested uses in-stream. `respan`: body-derived tokens
    /// take the use-site span (R10); argument streams pass false.
    fn walk(
        &mut self,
        stream: &[Spanned<Token>],
        args: &HashMap<&str, Vec<Spanned<Token>>>,
        respan: bool,
        out: &mut Vec<Spanned<Token>>,
    ) -> Result<()> {
        let mut cursor = TokenVecWrapper::new(stream).peekable();
        while let Some(next) = cursor.next() {
            let spanned = next?;
            let Token::Id(st) = &spanned.1 else {
                self.emit(spanned, respan, out)?;
                continue;
            };
            if let Some(arg_tokens) = args.get(st.1.as_str()) {
                // A parameter: already expanded, spans kept (R10).
                for t in arg_tokens {
                    self.emit(t.clone(), false, out)?;
                }
                continue;
            }
            match self.defines.get_key_value(&st.1) {
                None => self.emit(spanned, respan, out)?,
                Some((name, _)) if self.hide.contains(&name.as_str()) => {
                    // Hidden: a plain identifier (R4).
                    self.emit(spanned, respan, out)?;
                }
                Some((_, Define::Function(_)))
                    if !matches!(cursor.peek(), Some(Ok((_, Token::LParen(_), _)))) =>
                {
                    // Function-macro name, no `(`: a plain identifier (R2).
                    self.emit(spanned, respan, out)?;
                }
                Some((name, _)) => self.expand_named(name, &mut cursor, out)?,
            }
        }
        Ok(())
    }

    /// Capture a call's arguments from `cursor`: paren depth only, commas
    /// split at depth 1, newlines skipped (R6). The opening paren is
    /// already consumed.
    fn capture_arguments<T>(
        &mut self,
        name: &str,
        cursor: &mut Peekable<T>,
    ) -> Result<Vec<Vec<Spanned<Token>>>>
    where
        T: Iterator<Item = Result<Spanned<Token>>>,
    {
        let mut depth = 1_usize;
        let mut args: Vec<Vec<Spanned<Token>>> = vec![];
        let mut arg: Vec<Spanned<Token>> = vec![];

        loop {
            let Some(next) = cursor.next() else {
                return Err(self.unterminated(name));
            };
            let spanned = next?;
            match &spanned.1 {
                Token::LParen(_) => {
                    depth += 1;
                    arg.push(spanned);
                }
                Token::RParen(_) => {
                    depth -= 1;
                    if depth == 0 {
                        args.push(arg);
                        return Ok(args);
                    }
                    arg.push(spanned);
                }
                Token::Comma(_) if depth == 1 => {
                    args.push(arg);
                    arg = vec![];
                }
                Token::NewLine(_) => { /* calls may span lines */ }
                t if is_directive(t) => return Err(self.unterminated(name)),
                _ => arg.push(spanned),
            }
        }
    }

    /// C99 arity (R7): a zero-parameter macro's `()` is zero arguments; on
    /// one-plus parameters `()` is one empty argument. Each argument is
    /// fully expanded before substitution (R3), under the hide set in
    /// force at the call.
    fn bind_arguments(
        &mut self,
        name: &'a str,
        function: &'a FunctionMacro,
        mut raw: Vec<Vec<Spanned<Token>>>,
    ) -> Result<HashMap<&'a str, Vec<Spanned<Token>>>> {
        if function.args.is_empty() && raw.len() == 1 && raw[0].is_empty() {
            raw.clear();
        }
        if raw.len() != function.args.len() {
            let n = function.args.len();
            let plural = if n == 1 { "" } else { "s" };
            return Err(lpc_error!(
                Some(self.use_span),
                "macro `{}` takes {} argument{}, {} given",
                name,
                n,
                plural,
                raw.len()
            ));
        }
        let mut map = HashMap::new();
        for (param, tokens) in function.args.iter().zip(raw) {
            let mut expanded = vec![];
            self.walk(&tokens, &no_args(), false, &mut expanded)?;
            map.insert(param.as_str(), expanded);
        }
        Ok(map)
    }

    /// Emit one token, budgeted (R5) and respanned (R10).
    fn emit(
        &mut self,
        spanned: Spanned<Token>,
        respan: bool,
        out: &mut Vec<Spanned<Token>>,
    ) -> Result<()> {
        self.emitted += 1;
        if self.emitted > MAX_EXPANDED_TOKENS {
            return Err(lpc_error!(
                Some(self.use_span),
                "expansion of `{}` produces too many tokens (limit {})",
                self.top,
                MAX_EXPANDED_TOKENS
            ));
        }
        if respan {
            let (_, tok, _) = spanned;
            out.push((
                self.use_span.l(),
                tok.with_span(self.use_span),
                self.use_span.r(),
            ));
        } else {
            out.push(spanned);
        }
        Ok(())
    }

    fn unterminated(&self, name: &str) -> lpc_rs_errors::LpcError {
        lpc_error!(Some(self.use_span), "unterminated call to macro `{}`", name)
    }
}

/// The whole-line directive tokens; one inside argument capture means the
/// call ran into a directive line (R6).
fn is_directive(token: &Token) -> bool {
    matches!(
        token,
        Token::LocalInclude(_)
            | Token::SysInclude(_)
            | Token::PreprocessorIf(_)
            | Token::IfDef(_)
            | Token::IfNDef(_)
            | Token::PreprocessorElse(_)
            | Token::Endif(_)
            | Token::Define(_)
            | Token::Undef(_)
            | Token::Pragma(_)
    )
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::compiler::{
        compilation_context::CompilationContextBuilder, lexer::LexWrapper,
        preprocessor::Preprocessor,
    };

    /// Build a define table by scanning `defines` through the real handlers.
    async fn table(defines: &str) -> Preprocessor {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .build()
            .unwrap();
        let context = CompilationContextBuilder::default()
            .filename(Arc::new("expand_test.c".into()))
            .config(config)
            .build()
            .unwrap();
        let mut preprocessor = Preprocessor::new(context);
        preprocessor.scan("/expand_test.c", defines).await.unwrap();
        preprocessor
    }

    /// Drive the engine over `src` the way internal_scan does; Display strings out.
    fn expand_all(preprocessor: &Preprocessor, src: &str) -> Result<Vec<String>> {
        let expander = Expander::new(&preprocessor.defines);
        let mut cursor = LexWrapper::new(src).peekable();
        let mut out = vec![];
        while let Some(next) = cursor.next() {
            let spanned = next?;
            match &spanned.1 {
                Token::Id(st) => match expander.expand_use(st, &mut cursor)? {
                    Some(tokens) => out.extend(tokens),
                    None => out.push(spanned),
                },
                Token::NewLine(_) => {}
                _ => out.push(spanned),
            }
        }
        Ok(out.iter().map(|s| s.1.to_string()).collect())
    }

    #[tokio::test]
    async fn self_referential_macro_emits_its_own_name() {
        let pp = table("#define A A\n").await;
        assert_eq!(expand_all(&pp, "A").unwrap(), &["A"]);
    }

    #[tokio::test]
    async fn mutual_recursion_terminates() {
        let pp = table("#define A B\n#define B A\n").await;
        assert_eq!(expand_all(&pp, "A").unwrap(), &["A"]);
    }

    #[tokio::test]
    async fn function_macro_inside_object_body_expands() {
        let pp = table(
            "#define TP this_player()\n#define ENV(o) environment(o)\n#define HERE ENV(TP)\n",
        )
        .await;
        assert_eq!(
            expand_all(&pp, "HERE").unwrap(),
            &["environment", "(", "this_player", "(", ")", ")"]
        );
    }

    #[tokio::test]
    async fn bare_function_macro_name_is_an_identifier() {
        let pp = table("#define F(x) x\n").await;
        assert_eq!(expand_all(&pp, "F ;").unwrap(), &["F", ";"]);
    }

    #[tokio::test]
    async fn width_bomb_hits_the_cap() {
        let mut defines = String::from("#define X0 x\n");
        for i in 1..=17 {
            defines.push_str(&format!("#define X{i} X{} X{}\n", i - 1, i - 1));
        }
        let pp = table(&defines).await;
        let e = expand_all(&pp, "X17").unwrap_err();
        assert!(
            e.message()
                .contains("expansion of `X17` produces too many tokens")
        );
    }

    #[tokio::test]
    async fn directive_inside_arguments_is_unterminated() {
        let pp = table("#define F(a, b) a b\n").await;
        let e = expand_all(&pp, "F(1,\n#define X 1\n2)").unwrap_err();
        assert!(e.message().contains("unterminated call to macro `F`"));
    }

    #[tokio::test]
    async fn eof_inside_arguments_is_unterminated() {
        let pp = table("#define F(a, b) a b\n").await;
        let e = expand_all(&pp, "F(1").unwrap_err();
        assert!(e.message().contains("unterminated call to macro `F`"));
    }

    #[tokio::test]
    async fn zero_parameter_call_is_legal() {
        let pp = table("#define F() 5\n").await;
        assert_eq!(expand_all(&pp, "F()").unwrap(), &["5"]);
    }

    #[tokio::test]
    async fn zero_parameter_call_with_an_argument_errors() {
        let pp = table("#define F() 5\n").await;
        let e = expand_all(&pp, "F(x)").unwrap_err();
        assert!(e.message().contains("macro `F` takes 0 arguments, 1 given"));
    }

    #[tokio::test]
    async fn empty_parens_on_one_parameter_are_one_empty_argument() {
        let pp = table("#define G(x) a x b\n").await;
        assert_eq!(expand_all(&pp, "G()").unwrap(), &["a", "b"]);
    }

    #[tokio::test]
    async fn trailing_comma_is_a_second_empty_argument() {
        let pp = table("#define H(x, y) x 9 y\n").await;
        assert_eq!(expand_all(&pp, "H(1,)").unwrap(), &["1", "9"]);
    }

    #[tokio::test]
    async fn arity_mismatch_names_the_counts() {
        let pp = table("#define BAR(a, b) (a - b)\n").await;
        let e = expand_all(&pp, "BAR(34)").unwrap_err();
        assert!(
            e.message()
                .contains("macro `BAR` takes 2 arguments, 1 given")
        );
    }

    #[tokio::test]
    async fn parens_protect_argument_commas() {
        let pp = table("#define F(a) a\n").await;
        assert_eq!(
            expand_all(&pp, "F( ({ 1, 2 }) )").unwrap(),
            &["(", "{", "1", ",", "2", "}", ")"]
        );
    }

    #[tokio::test]
    async fn empty_body_macro_expands_to_nothing() {
        let pp = table("#define FOO\n").await;
        assert_eq!(expand_all(&pp, "x FOO y").unwrap(), &["x", "y"]);
    }

    #[tokio::test]
    async fn body_tokens_take_the_use_site_span() {
        let pp = table("#define FOO 42\n").await;
        let expander = Expander::new(&pp.defines);
        let mut cursor = LexWrapper::new("FOO").peekable();
        let Some(Ok((_, Token::Id(st), _))) = cursor.next() else {
            panic!("expected an Id");
        };
        let use_span = st.0;
        let mut tokens = expander.expand_use(&st, &mut cursor).unwrap().unwrap();
        assert_eq!(*tokens[0].1.span_ref().unwrap(), use_span);
    }

    #[tokio::test]
    async fn argument_tokens_keep_their_own_spans() {
        let pp = table("#define ID(x) x\n").await;
        let expander = Expander::new(&pp.defines);
        let mut cursor = LexWrapper::new("ID(marf)").peekable();
        let Some(Ok((_, Token::Id(st), _))) = cursor.next() else {
            panic!("expected an Id");
        };
        let use_span = st.0;
        let mut tokens = expander.expand_use(&st, &mut cursor).unwrap().unwrap();
        let got = *tokens[0].1.span_ref().unwrap();
        assert_ne!(got, use_span);
        assert_eq!(tokens[0].1.to_string(), "marf");
    }
}
