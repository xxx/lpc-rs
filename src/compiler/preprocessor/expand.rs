//! The expansion engine: the one rewriter of token streams against the
//! define table. One cursor per stream, non-reexpansion via the hide set,
//! an output cap, paren-balanced argument capture.

use std::{collections::HashMap, iter::Peekable};

use lpc_rs_errors::{Result, lpc_error, span::Span};

use crate::compiler::{
    lexer::{Spanned, Token, TokenVecWrapper, logos_token::StringToken},
    preprocessor::define::{Define, FunctionMacro},
};

/// Hard cap on tokens one top-level expansion may emit (R5): the hide set
/// bounds re-expansion of names, this bounds width. A work bound on what
/// one top-level expansion may materialize — `substitute`'s construction
/// and `emit`'s rescan both count against it — conservative, not an exact
/// count of the final output.
const MAX_EXPANDED_TOKENS: usize = 65_536;

/// Hard cap on nested invocation depth (R5): source nesting like
/// `F(F(F(...)))` recurses once per level and isn't bounded by the hide
/// set at all — only this stops it before the real call stack would. The
/// same bound guards recursion generally: nested invocations, long alias
/// chains, and (mod.rs's `#if` resolvers) define chains too.
pub(super) const MAX_EXPANSION_DEPTH: usize = 256;

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
            depth: 0,
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
    /// Current nested-invocation depth; see `MAX_EXPANSION_DEPTH`.
    depth: usize,
}

/// No parameters to substitute: object-macro bodies have none.
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
        if self.depth >= MAX_EXPANSION_DEPTH {
            return Err(self.too_deep());
        }
        self.depth += 1;
        let result = self.expand_named_at_depth(name, cursor, out);
        self.depth -= 1;
        result
    }

    /// Substitute-then-rescan (C99): build the replacement list — body
    /// tokens with parameters spliced in and everything else respanned to
    /// the use site — then walk it as one stream.
    fn expand_named_at_depth<T>(
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
                let replacement = self.substitute(&object.tokens, &no_args())?;
                self.hide.push(name);
                let walked = self.walk(&replacement, out);
                self.hide.pop();
                walked
            }
            Define::Function(function) => {
                cursor.next(); // the `(` the caller peeked
                let raw = self.capture_arguments(name, cursor)?;
                let args = self.bind_arguments(name, function, raw)?;
                let replacement = self.substitute(&function.tokens, &args)?;
                self.hide.push(name);
                let walked = self.walk(&replacement, out);
                self.hide.pop();
                walked
            }
        }
    }

    /// Build one macro's replacement from its raw body: a parameter Id
    /// splices its bound (already-expanded) tokens verbatim, their own
    /// spans kept; every other token is respanned to the use site here, at
    /// construction (R10) — once, not on every rescan. Budgeted against
    /// `MAX_EXPANDED_TOKENS` here too (R5): a parameter splice can multiply
    /// body-occurrences by argument size, so `walk`/`emit`'s cap alone
    /// would let construction build an unbounded vector first.
    fn substitute(
        &self,
        body: &[Spanned<Token>],
        args: &HashMap<&str, Vec<Spanned<Token>>>,
    ) -> Result<Vec<Spanned<Token>>> {
        let mut replacement = Vec::with_capacity(body.len());
        for spanned in body {
            if let Token::Id(st) = &spanned.1
                && let Some(arg_tokens) = args.get(st.1.as_str())
            {
                if self.emitted + replacement.len() + arg_tokens.len() > MAX_EXPANDED_TOKENS {
                    return Err(self.too_many());
                }
                replacement.extend(arg_tokens.iter().cloned());
                continue;
            }
            if self.emitted + replacement.len() + 1 > MAX_EXPANDED_TOKENS {
                return Err(self.too_many());
            }
            let tok = spanned.1.clone();
            replacement.push((
                self.use_span.l(),
                tok.with_span(self.use_span),
                self.use_span.r(),
            ));
        }
        Ok(replacement)
    }

    /// Rescan one already-substituted, already-spanned replacement (R2):
    /// hidden names and non-macro tokens emit verbatim; a live macro name
    /// expands in place, off this same cursor — so a nested call's
    /// argument capture sees the substitution `substitute` already made.
    fn walk(&mut self, stream: &[Spanned<Token>], out: &mut Vec<Spanned<Token>>) -> Result<()> {
        let mut cursor = TokenVecWrapper::new(stream).peekable();
        while let Some(next) = cursor.next() {
            let spanned = next?;
            let Token::Id(st) = &spanned.1 else {
                self.emit(spanned, out)?;
                continue;
            };
            match self.defines.get_key_value(&st.1) {
                None => self.emit(spanned, out)?,
                Some((name, _)) if self.hide.contains(&name.as_str()) => {
                    // Hidden: a plain identifier (R4).
                    self.emit(spanned, out)?;
                }
                Some((_, Define::Function(_)))
                    if !matches!(cursor.peek(), Some(Ok((_, Token::LParen(_), _)))) =>
                {
                    // Function-macro name, no `(`: a plain identifier (R2).
                    self.emit(spanned, out)?;
                }
                Some((name, _)) => self.expand_named(name, &mut cursor, out)?,
            }
        }
        Ok(())
    }

    /// Capture a call's arguments from `cursor`: paren depth only, commas
    /// split at depth 1; calls may span lines, since newlines never
    /// tokenize (R6). The opening paren is already consumed.
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
                t if is_directive(t) => return Err(self.unterminated(name)),
                _ => arg.push(spanned),
            }
        }
    }

    /// C99 arity (R7): a zero-parameter macro's `()` is zero arguments; on
    /// one-plus parameters `()` is one empty argument. Each argument is
    /// fully expanded before substitution (R3), under the hide set in
    /// force at the call — `name` itself isn't pushed onto it until after
    /// this returns, so a same-named call inside an argument expands freely.
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
            self.walk(&tokens, &mut expanded)?;
            map.insert(param.as_str(), expanded);
        }
        Ok(map)
    }

    /// Emit one token, budgeted (R5). Its span is already final —
    /// `substitute` set it once, at replacement construction (R10).
    fn emit(&mut self, spanned: Spanned<Token>, out: &mut Vec<Spanned<Token>>) -> Result<()> {
        self.emitted += 1;
        if self.emitted > MAX_EXPANDED_TOKENS {
            return Err(self.too_many());
        }
        out.push(spanned);
        Ok(())
    }

    /// The "unterminated call" diagnostic (R6), naming the macro being called.
    fn unterminated(&self, name: &str) -> lpc_rs_errors::LpcError {
        lpc_error!(Some(self.use_span), "unterminated call to macro `{}`", name)
    }

    /// The "nests too deeply" diagnostic (R5), naming the top-level macro.
    fn too_deep(&self) -> lpc_rs_errors::LpcError {
        lpc_error!(
            Some(self.use_span),
            "expansion of `{}` nests too deeply (limit {})",
            self.top,
            MAX_EXPANSION_DEPTH
        )
    }

    /// The "produces too many tokens" diagnostic (R5), naming the top-level
    /// macro; shared by `emit`'s rescan-time check and `substitute`'s
    /// construction-time one.
    fn too_many(&self) -> lpc_rs_errors::LpcError {
        lpc_error!(
            Some(self.use_span),
            "expansion of `{}` produces too many tokens (limit {})",
            self.top,
            MAX_EXPANDED_TOKENS
        )
    }
}

/// A directive line inside argument capture means the call ran into a
/// directive (R6) — the capture is unterminated.
fn is_directive(token: &Token) -> bool {
    matches!(token, Token::DirectiveLine(_))
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
    async fn splice_bomb_hits_the_cap_without_materializing_it() {
        // DUP's body repeats its parameter 1,000 times; the argument is
        // 1,000 tokens. Unbudgeted, `substitute` would splice 1,000,000
        // tokens before `walk`/`emit` ever see them. Budgeted, it must
        // bail during construction — this test itself must stay fast.
        let body = "x ".repeat(1_000);
        let defines = format!("#define DUP(x) {}\n", body.trim_end());
        let pp = table(&defines).await;
        let arg = "1 ".repeat(1_000);
        let src = format!("DUP({})", arg.trim_end());

        let start = std::time::Instant::now();
        let e = expand_all(&pp, &src).unwrap_err();
        assert!(start.elapsed() < std::time::Duration::from_secs(2));

        assert!(
            e.message()
                .contains("expansion of `DUP` produces too many tokens")
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
    async fn a_parameter_used_as_a_nested_calls_argument_is_substituted() {
        let pp = table("#define ENV(o) environment(o)\n#define WRAP(a) ENV(a)\n").await;
        assert_eq!(
            expand_all(&pp, "WRAP(this_player())").unwrap(),
            &["environment", "(", "this_player", "(", ")", ")"]
        );
    }

    #[tokio::test]
    async fn hide_set_does_not_cover_argument_pre_expansion() {
        let pp = table("#define F(x) x\n").await;
        assert_eq!(expand_all(&pp, "F(F(1))").unwrap(), &["1"]);
    }

    #[tokio::test]
    async fn an_object_macros_body_does_not_reach_past_its_own_tokens_for_a_paren() {
        let pp = table("#define A F\n#define F(x) x\n").await;
        assert_eq!(expand_all(&pp, "A (1)").unwrap(), &["F", "(", "1", ")"]);
    }

    #[tokio::test]
    async fn brackets_protect_argument_commas() {
        let pp = table("#define F(a) a\n").await;
        assert_eq!(
            expand_all(&pp, "F( ([ 1, 2 ]) )").unwrap(),
            &["(", "[", "1", ",", "2", "]", ")"]
        );
    }

    #[tokio::test]
    async fn deep_nesting_hits_the_depth_cap() {
        let pp = table("#define F(x) x\n").await;
        let src = format!("{}1{}", "F(".repeat(300), ")".repeat(300));
        let e = expand_all(&pp, &src).unwrap_err();
        assert!(e.message().contains("nests too deeply (limit 256)"));
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
        assert_eq!(tokens[0].0, use_span.l());
        assert_eq!(tokens[0].2, use_span.r());
    }

    #[tokio::test]
    async fn argument_tokens_keep_their_own_spans() {
        let pp = table("#define ID(x) x\n").await;
        let expander = Expander::new(&pp.defines);
        let src = "ID(marf)";
        let mut cursor = LexWrapper::new(src).peekable();
        let Some(Ok((_, Token::Id(st), _))) = cursor.next() else {
            panic!("expected an Id");
        };
        let use_span = st.0;
        let mut tokens = expander.expand_use(&st, &mut cursor).unwrap().unwrap();
        let got = *tokens[0].1.span_ref().unwrap();
        assert_ne!(got, use_span);
        assert_eq!(tokens[0].1.to_string(), "marf");

        // Not just "not the use span" — exactly what a fresh lex of the
        // same source gives the "marf" token.
        let mut fresh = LexWrapper::new(src);
        let marf_span = loop {
            match fresh.next().unwrap().unwrap() {
                (_, Token::Id(marf_st), _) if marf_st.1 == "marf" => break marf_st.0,
                _ => continue,
            }
        };
        assert_eq!(got, marf_span);
    }
}
