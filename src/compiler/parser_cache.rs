use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    mem::{discriminant, size_of},
    num::NonZeroUsize,
    sync::Arc,
};

use lpc_rs_errors::{LpcError, Result, source_map::DiagnosticSources, span::HasSpan};
use lru::LruCache;
use parking_lot::Mutex;

use super::{
    ast::{ast_node::AstNode, expression_node::ExpressionNode, program_node::ProgramNode},
    compilation_context::CompilationContext,
    lexer::{Token, TokenTriples},
};

const MAX_ENTRIES: usize = 256;
const MAX_BYTES: usize = 64 * 1024 * 1024;

/// Bounded reuse of pristine syntax trees after each fresh preprocessing walk.
#[derive(Debug, Default)]
pub struct ParserCache {
    entries: Mutex<Entries>,
    #[cfg(test)]
    disabled: std::sync::atomic::AtomicBool,
    #[cfg(test)]
    hits: std::sync::atomic::AtomicUsize,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Policy {
    strict: Vec<(usize, usize)>,
    closure_count: u16,
    capture_count: usize,
}

impl Policy {
    fn new(context: &CompilationContext) -> Self {
        let mut strict: Vec<_> = context
            .strict_types_from
            .iter()
            .map(|(&file, &offset)| (file, offset))
            .collect();
        strict.sort_unstable();
        Self {
            strict,
            closure_count: context.closure_count,
            capture_count: context.capture_count,
        }
    }
}

#[derive(Debug)]
struct Parsed {
    tokens: Vec<Token>,
    policy: Policy,
    sources: DiagnosticSources,
    node: ProgramNode,
    closure_count: u16,
    capture_count: usize,
    warnings: Vec<LpcError>,
    cost: usize,
}

#[derive(Debug)]
struct Entries {
    values: LruCache<u64, Arc<Parsed>>,
    bytes: usize,
}

impl Default for Entries {
    fn default() -> Self {
        Self {
            values: LruCache::new(NonZeroUsize::new(MAX_ENTRIES).unwrap()),
            bytes: 0,
        }
    }
}

fn fingerprint(tokens: &[Token], policy: &Policy) -> u64 {
    let mut hash = DefaultHasher::new();
    policy.hash(&mut hash);
    tokens.len().hash(&mut hash);
    for token in tokens {
        discriminant(token).hash(&mut hash);
        token.span().hash(&mut hash);
        match token {
            Token::StringLiteral(value)
            | Token::Id(value)
            | Token::ClosureArgVar(value)
            | Token::DirectiveLine(value) => value.1.hash(&mut hash),
            Token::IntLiteral(value) => value.1.hash(&mut hash),
            Token::FloatLiteral(value) => value.1.to_bits().hash(&mut hash),
            _ => {}
        }
    }
    hash.finish()
}

fn retained_cost(
    tokens: &[Token],
    token_capacity: usize,
    policy: &Policy,
    warnings: &[LpcError],
) -> usize {
    // Allow several syntax nodes per token, including spare vector capacity and generated names.
    let node_allowance = 2 * (size_of::<AstNode>() + size_of::<ExpressionNode>());
    let strings = tokens.iter().fold(0usize, |bytes, token| {
        let text = match token {
            Token::StringLiteral(value)
            | Token::Id(value)
            | Token::ClosureArgVar(value)
            | Token::DirectiveLine(value) => value.1.len(),
            _ => 0,
        };
        bytes.saturating_add(text.saturating_mul(4))
    });
    size_of::<Parsed>()
        .saturating_add(token_capacity.saturating_mul(size_of::<Token>()))
        .saturating_add(tokens.len().saturating_mul(node_allowance))
        .saturating_add(strings)
        .saturating_add(
            policy
                .strict
                .capacity()
                .saturating_mul(size_of::<(usize, usize)>()),
        )
        .saturating_add(warnings.len().saturating_mul(1024))
}

impl ParserCache {
    #[cfg(test)]
    pub(crate) fn disable(&self) {
        self.disabled
            .store(true, std::sync::atomic::Ordering::Relaxed);
    }

    fn get(
        &self,
        key: u64,
        tokens: &[Token],
        policy: &Policy,
        sources: &DiagnosticSources,
    ) -> Option<Arc<Parsed>> {
        let entry = self.entries.lock().values.get(&key).cloned()?;
        (entry.sources.same_storage(sources) && entry.policy == *policy && entry.tokens == tokens)
            .then_some(entry)
    }

    fn insert(&self, key: u64, entry: Arc<Parsed>) {
        if entry.cost > MAX_BYTES {
            return;
        }
        let mut entries = self.entries.lock();
        if let Some(old) = entries.values.pop(&key) {
            entries.bytes -= old.cost;
        }
        while entries.bytes + entry.cost > MAX_BYTES || entries.values.len() == MAX_ENTRIES {
            let Some((_, old)) = entries.values.pop_lru() else {
                break;
            };
            entries.bytes -= old.cost;
        }
        entries.bytes += entry.cost;
        entries.values.put(key, entry);
    }

    pub(crate) fn parse(
        &self,
        tokens: Vec<Token>,
        context: &mut CompilationContext,
    ) -> Result<ProgramNode> {
        #[cfg(test)]
        let enabled = !self.disabled.load(std::sync::atomic::Ordering::Relaxed);
        #[cfg(not(test))]
        let enabled = true;
        if !enabled {
            return parse_uncached(&tokens, context);
        }
        let policy = Policy::new(context);
        let key = fingerprint(&tokens, &policy);
        if let Some(entry) = self.get(key, &tokens, &policy, &context.diagnostic_sources) {
            #[cfg(test)]
            self.hits.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            context.closure_count = entry.closure_count;
            context.capture_count = entry.capture_count;
            for warning in &entry.warnings {
                context.diagnostics.record(warning.clone());
            }
            return Ok(entry.node.clone());
        }
        let warning_start = context.diagnostics.errors().len();
        let node = parse_uncached(&tokens, context)?;
        let warnings = &context.diagnostics.errors()[warning_start..];
        let cost = retained_cost(&tokens, tokens.capacity(), &policy, warnings);
        if cost <= MAX_BYTES {
            self.insert(
                key,
                Arc::new(Parsed {
                    tokens,
                    policy,
                    sources: context.diagnostic_sources.clone(),
                    node: node.clone(),
                    closure_count: context.closure_count,
                    capture_count: context.capture_count,
                    warnings: warnings.to_vec(),
                    cost,
                }),
            );
        }
        Ok(node)
    }
}

fn parse_uncached(tokens: &[Token], context: &mut CompilationContext) -> Result<ProgramNode> {
    // Lalrpop's EOF locations lack file IDs, so unlocated errors use the last token's span.
    crate::lpc_parser::ProgramParser::new()
        .parse(context, TokenTriples::new(tokens))
        .map_err(|error| LpcError::from(error).or_span(tokens.last().map(HasSpan::span)))
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

    use async_trait::async_trait;
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_errors::span::Span;

    use super::*;
    use crate::{
        compiler::{CompilerBuilder, compile_gate::CompileGate, preprocessor::Preprocessor},
        test_support::{TempLib, temp_lib_config},
    };

    fn compiler(root: &TempLib, cache: Arc<ParserCache>) -> super::super::Compiler {
        CompilerBuilder::default()
            .config(temp_lib_config(root))
            .parser_cache(cache)
            .build()
            .unwrap()
    }

    #[tokio::test]
    async fn hits_restore_generated_names_and_warnings_without_retaining_walked_syntax() {
        let root = TempLib::new("parser-generated-names");
        let cache = Arc::new(ParserCache::default());
        let compiler = compiler(&root, cache.clone());
        let code = "int forward(int); function make(int n) { return (: $(n) + 1 :); }";
        let path = LpcPath::from("/main.c");
        let (expected, context) = compiler.parse_string(&path, code).await.unwrap();
        assert_eq!((context.closure_count, context.capture_count), (1, 1));
        assert_eq!(context.diagnostics.errors().len(), 1);
        for _ in 0..2 {
            let compiled = compiler.compile_string("/main.c", code).await.unwrap();
            assert_eq!(compiled.warnings.last().unwrap().warnings.len(), 1);
            let (actual, context) = compiler.parse_string(&path, code).await.unwrap();
            assert_eq!(actual, expected);
            assert_eq!((context.closure_count, context.capture_count), (1, 1));
            assert_eq!(
                context.diagnostics.errors()[0].message(),
                "function prototypes are ignored; the definition declares the function"
            );
        }
        assert_eq!(cache.hits.load(Ordering::Relaxed), 4);
    }

    #[tokio::test]
    async fn cached_parent_syntax_is_checked_against_each_receivers_signatures() {
        let root = TempLib::new("parser-receiver");
        std::fs::write(
            root.join("consumer.c"),
            "int result() { return supply(3); }",
        )
        .unwrap();
        let cache = Arc::new(ParserCache::default());
        let compiler = compiler(&root, cache.clone());
        compiler
            .compile_string(
                "/good.c",
                "inherit \"/consumer\"; int supply(int n) { return n + 4; }",
            )
            .await
            .unwrap();
        let error = compiler
            .compile_string(
                "/bad.c",
                "inherit \"/consumer\"; int supply(string n) { return 1; }",
            )
            .await
            .unwrap_err();
        assert!(
            error.message().contains("unexpected argument type"),
            "{error}"
        );
        assert_eq!(cache.hits.load(Ordering::Relaxed), 1);
    }

    #[tokio::test]
    async fn cached_class_names_resolve_current_inherited_fields() {
        let root = TempLib::new("parser-class-edits");
        let cache = Arc::new(ParserCache::default());
        let compiler = compiler(&root, cache.clone());
        let code = "inherit \"/parent\"; class item make() { return new(class item, value: 3); }";
        std::fs::write(root.join("parent.c"), "class item { int value; }").unwrap();
        compiler.compile_string("/child.c", code).await.unwrap();
        std::fs::write(root.join("parent.c"), "class item { string value; }").unwrap();
        let error = compiler.compile_string("/child.c", code).await.unwrap_err();
        assert_eq!(
            error.message(),
            "initializer `value` expects string, found int"
        );
        assert_eq!(cache.hits.load(Ordering::Relaxed), 1);
    }

    #[tokio::test]
    async fn identical_numeric_source_ids_in_different_storages_do_not_hit() {
        let root = TempLib::new("parser-source-storage");
        let cache = Arc::new(ParserCache::default());
        let first = CompilerBuilder::default()
            .config(temp_lib_config(&root))
            .parser_cache(cache.clone())
            .diagnostic_sources(DiagnosticSources::scoped())
            .build()
            .unwrap();
        let second = CompilerBuilder::default()
            .config(temp_lib_config(&root))
            .parser_cache(cache.clone())
            .diagnostic_sources(DiagnosticSources::scoped())
            .build()
            .unwrap();
        let path = LpcPath::from("/main.c");
        first.parse_string(&path, "int a;").await.unwrap();
        second.parse_string(&path, "int a;").await.unwrap();
        assert_eq!(cache.hits.load(Ordering::Relaxed), 0);
        second.parse_string(&path, "int a;").await.unwrap();
        assert_eq!(cache.hits.load(Ordering::Relaxed), 1);
    }

    #[tokio::test]
    async fn strict_policy_and_initial_name_counters_are_parser_inputs() {
        let root = TempLib::new("parser-context-inputs");
        let compiler = compiler(&root, Arc::default());
        let (tokens, preprocessor) = compiler
            .preprocess_string(LpcPath::from("/main.c"), "value(arg) { return arg; }")
            .await
            .unwrap();
        let cache = ParserCache::default();
        let mut context = preprocessor.into_context();
        cache.parse(tokens.clone(), &mut context).unwrap();
        context
            .strict_types_from
            .insert(tokens[0].span().file_id(), 0);
        assert!(
            cache
                .parse(tokens, &mut context)
                .unwrap_err()
                .message()
                .contains("Missing type for argument")
        );
        assert_eq!(cache.hits.load(Ordering::Relaxed), 0);
        context.strict_types_from.clear();
        let mut preprocessor = Preprocessor::new(context);
        let tokens = preprocessor
            .scan("/main.c", "function make(int n) { return (: $(n) :); }")
            .await
            .unwrap();
        let mut context = preprocessor.into_context();
        cache.parse(tokens.clone(), &mut context).unwrap();
        assert_eq!((context.closure_count, context.capture_count), (1, 1));
        cache.parse(tokens, &mut context).unwrap();
        assert_eq!((context.closure_count, context.capture_count), (2, 2));
        assert_eq!(cache.hits.load(Ordering::Relaxed), 0);
    }

    #[tokio::test]
    async fn preprocessing_repeats_warnings_and_observes_edited_headers_on_hits() {
        let root = TempLib::new("parser-header-edits");
        std::fs::write(root.join("value.h"), "#define VALUE 1\n").unwrap();
        let cache = Arc::new(ParserCache::default());
        let compiler = compiler(&root, cache.clone());
        let code = "#include \"value.h\"\n#pragma save_binary\nint result() { return VALUE; }";
        for _ in 0..2 {
            let compiled = compiler.compile_string("/main.c", code).await.unwrap();
            assert_eq!(compiled.warnings.last().unwrap().warnings.len(), 1);
        }
        assert_eq!(cache.hits.load(Ordering::Relaxed), 1);
        std::fs::write(root.join("value.h"), "#define VALUE \"text\"\n").unwrap();
        let error = compiler.compile_string("/main.c", code).await.unwrap_err();
        assert!(error.message().contains("type"), "{error}");
        assert_eq!(cache.hits.load(Ordering::Relaxed), 1);
    }

    #[derive(Debug, Default)]
    struct Gate {
        deny: AtomicBool,
        includes: AtomicUsize,
        inherits: AtomicUsize,
        overrides: AtomicUsize,
    }
    #[async_trait]
    impl CompileGate for Gate {
        async fn include(&self, _: &str, _: &str) -> Result<bool> {
            self.includes.fetch_add(1, Ordering::Relaxed);
            Ok(!self.deny.load(Ordering::Relaxed))
        }
        async fn inherit(&self, _: &str, _: &str) -> Result<bool> {
            self.inherits.fetch_add(1, Ordering::Relaxed);
            Ok(!self.deny.load(Ordering::Relaxed))
        }
        async fn efun_override(&self, _: &str, _: &str, _: &str) -> Result<bool> {
            self.overrides.fetch_add(1, Ordering::Relaxed);
            Ok(!self.deny.load(Ordering::Relaxed))
        }
    }

    #[tokio::test]
    async fn hits_keep_include_inherit_and_explicit_efun_authorization_per_use() {
        let root = TempLib::new("parser-permissions");
        std::fs::write(root.join("header.h"), "int header;\n").unwrap();
        std::fs::write(root.join("parent.c"), "int parent;\n").unwrap();
        let cache = Arc::new(ParserCache::default());
        let gate = Arc::new(Gate::default());
        let compiler = CompilerBuilder::default()
            .config(temp_lib_config(&root))
            .parser_cache(cache.clone())
            .gate(Some(gate.clone() as Arc<dyn CompileGate>))
            .build()
            .unwrap();
        let code = "inherit \"/parent\";\n#include \"header.h\"\n int value() { return efun::sizeof(\"x\"); }";
        for _ in 0..2 {
            compiler.compile_string("/main.c", code).await.unwrap();
        }
        assert_eq!(cache.hits.load(Ordering::Relaxed), 2);
        assert_eq!(gate.includes.load(Ordering::Relaxed), 2);
        assert_eq!(gate.inherits.load(Ordering::Relaxed), 2);
        assert_eq!(gate.overrides.load(Ordering::Relaxed), 2);
        gate.deny.store(true, Ordering::Relaxed);
        assert!(
            compiler
                .compile_string("/main.c", code)
                .await
                .unwrap_err()
                .message()
                .contains("permission denied")
        );
    }

    #[tokio::test]
    async fn cached_syntax_observes_new_inherit_and_explicit_efun_denials() {
        let root = TempLib::new("parser-policy-changes");
        std::fs::write(root.join("parent.c"), "int parent;").unwrap();
        for code in [
            "inherit \"/parent\";",
            "int value() { return efun::sizeof(\"x\"); }",
        ] {
            let cache = Arc::new(ParserCache::default());
            let gate = Arc::new(Gate::default());
            let compiler = CompilerBuilder::default()
                .config(temp_lib_config(&root))
                .parser_cache(cache.clone())
                .gate(Some(gate.clone() as Arc<dyn CompileGate>))
                .build()
                .unwrap();
            compiler.compile_string("/main.c", code).await.unwrap();
            gate.deny.store(true, Ordering::Relaxed);
            assert!(
                compiler
                    .compile_string("/main.c", code)
                    .await
                    .unwrap_err()
                    .message()
                    .contains("permission denied")
            );
            assert_eq!(cache.hits.load(Ordering::Relaxed), 1);
        }
    }

    #[tokio::test]
    async fn parse_failures_are_not_retained_and_disabled_cache_runs_the_parser() {
        let root = TempLib::new("parser-failures");
        let cache = Arc::new(ParserCache::default());
        let compiler = compiler(&root, cache.clone());
        for _ in 0..2 {
            assert!(
                compiler
                    .parse_string(&LpcPath::from("/bad.c"), "int value() {")
                    .await
                    .is_err()
            );
        }
        assert!(cache.entries.lock().values.is_empty());
        let code = "int value;";
        let path = LpcPath::from("/good.c");
        let expected = compiler.parse_string(&path, code).await.unwrap().0;
        cache.disable();
        let actual = compiler.parse_string(&path, code).await.unwrap().0;
        assert_eq!(actual, expected);
        assert_eq!(cache.hits.load(Ordering::Relaxed), 0);
    }

    #[tokio::test]
    async fn fingerprint_collisions_and_warning_spans_are_compared_exactly() {
        let root = TempLib::new("parser-collisions");
        let cache = Arc::new(ParserCache::default());
        let compiler = compiler(&root, cache.clone());
        let (tokens, preprocessor) = compiler
            .preprocess_string(LpcPath::from("/main.c"), "int value;")
            .await
            .unwrap();
        let mut context = preprocessor.into_context();
        let policy = Policy::new(&context);
        let key = fingerprint(&tokens, &policy);
        cache.parse(tokens.clone(), &mut context).unwrap();
        let mut changed = tokens.clone();
        *changed[0].span_ref() =
            Span::new(tokens[0].span().file_id(), 0..3).with_warnings_suppressed(true);
        assert!(
            cache
                .get(key, &changed, &policy, &context.diagnostic_sources)
                .is_none()
        );
        assert_ne!(fingerprint(&changed, &policy), key);
        changed[0] = Token::String(tokens[0].span());
        assert!(
            cache
                .get(key, &changed, &policy, &context.diagnostic_sources)
                .is_none()
        );
    }

    #[test]
    fn cache_evicts_by_recency_and_estimated_bytes_and_rejects_oversized_entries() {
        let cache = ParserCache::default();
        let entry = |cost| {
            Arc::new(Parsed {
                tokens: vec![],
                policy: Policy::new(&CompilationContext::default()),
                sources: DiagnosticSources::default(),
                node: ProgramNode::default(),
                closure_count: 0,
                capture_count: 0,
                warnings: vec![],
                cost,
            })
        };
        for i in 0..MAX_ENTRIES {
            cache.insert(i as u64, entry(1));
        }
        cache.entries.lock().values.get(&0);
        cache.insert(MAX_ENTRIES as u64, entry(1));
        assert!(cache.entries.lock().values.contains(&0));
        assert!(!cache.entries.lock().values.contains(&1));
        cache.insert(999, entry(MAX_BYTES));
        assert_eq!(cache.entries.lock().values.len(), 1);
        assert_eq!(cache.entries.lock().bytes, MAX_BYTES);
        cache.insert(1000, entry(MAX_BYTES + 1));
        assert_eq!(cache.entries.lock().values.len(), 1);
        cache.insert(999, entry(1));
        assert_eq!(cache.entries.lock().bytes, 1);
    }
}
