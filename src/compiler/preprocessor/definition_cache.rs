use std::{mem::size_of, sync::Arc, time::Duration};

use lpc_rs_errors::{source_map::DiagnosticSources, span::Span};
use lpc_rs_utils::config::Config;

use super::{define::Define, preprocessor_node::PreprocessorNode};
use crate::compiler::{
    cache::{CacheStats, RetainedCache},
    lexer::Token,
};

/// Reuses pure macro preparation without retaining a compile's macro environment.
#[derive(Debug)]
pub struct DefinitionCache {
    entries: RetainedCache<Span, Definition>,
    #[cfg(test)]
    disabled: std::sync::atomic::AtomicBool,
}

#[derive(Debug)]
pub(super) struct Definition {
    pub name: String,
    pub replacement: Define,
    sources: DiagnosticSources,
}

impl Default for DefinitionCache {
    fn default() -> Self {
        Self::new(&Config::default())
    }
}

impl DefinitionCache {
    /// Construct a cache using this compiler or VM's fixed configuration.
    pub fn new(config: &Config) -> Self {
        Self {
            entries: RetainedCache::new(
                config.definition_cache_max_entries,
                config.definition_cache_max_bytes,
                Duration::from_secs(config.compiler_cache_idle_time),
            ),
            #[cfg(test)]
            disabled: Default::default(),
        }
    }

    pub(crate) fn expire(&self) -> usize {
        self.entries.expire()
    }

    /// Current retention charges and cumulative lookup counts.
    pub fn stats(&self) -> CacheStats {
        self.entries.stats()
    }

    #[cfg(test)]
    pub(crate) fn disable(&self) {
        self.disabled
            .store(true, std::sync::atomic::Ordering::Relaxed);
    }

    pub(super) fn get(&self, span: Span, sources: &DiagnosticSources) -> Option<Arc<Definition>> {
        #[cfg(test)]
        if self.disabled.load(std::sync::atomic::Ordering::Relaxed) {
            return None;
        }
        if !self.entries.accepts(0) {
            return None;
        }
        self.entries
            .get_if(&span, |entry| entry.sources.same_storage(sources))
    }

    pub(super) fn insert(
        &self,
        span: Span,
        text_bytes: usize,
        name: &str,
        replacement: &Define,
        sources: &DiagnosticSources,
    ) {
        #[cfg(test)]
        if self.disabled.load(std::sync::atomic::Ordering::Relaxed) {
            return;
        }
        if !self.entries.accepts(0) {
            return;
        }
        let (tokens, argument_bytes) = match replacement {
            Define::Object(value) => (&value.tokens, 0),
            Define::Function(value) => (
                &value.tokens,
                value.args.iter().fold(
                    value.args.capacity() * size_of::<String>(),
                    |bytes, name| bytes.saturating_add(name.capacity()),
                ),
            ),
        };
        // Token strings and expression nodes are bounded conservatively by the directive's text and tokens.
        let cost = size_of::<Definition>()
            .saturating_add(argument_bytes)
            .saturating_add(text_bytes.saturating_mul(4))
            .saturating_add(tokens.capacity().saturating_mul(size_of::<Token>()))
            .saturating_add(
                tokens
                    .len()
                    .saturating_mul(2 * size_of::<PreprocessorNode>()),
            );
        if !self.entries.accepts(cost) {
            return;
        }
        self.entries.insert(
            span,
            Arc::new(Definition {
                name: name.to_owned(),
                replacement: replacement.clone(),
                sources: sources.clone(),
            }),
            cost,
        );
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

    use async_trait::async_trait;
    use lpc_rs_errors::Result;

    use super::*;
    use crate::{
        compiler::{
            CompilerBuilder, compilation_context::CompilationContextBuilder,
            compile_gate::CompileGate, preprocessor::Preprocessor, source::CompilerSource,
        },
        test_support::{TempLib, temp_lib_config},
    };

    async fn tokens(
        root: &TempLib,
        cache: Arc<DefinitionCache>,
        sources: DiagnosticSources,
        code: &str,
    ) -> Vec<String> {
        let config = temp_lib_config(root);
        let context = CompilationContextBuilder::default()
            .source(CompilerSource::new("/main.c", &config))
            .config(config)
            .definition_cache(cache)
            .diagnostic_sources(sources)
            .build()
            .unwrap();
        Preprocessor::new(context)
            .scan("/main.c", code)
            .await
            .unwrap()
            .iter()
            .map(ToString::to_string)
            .collect()
    }

    #[tokio::test]
    async fn cached_macros_use_the_current_environment_and_conditional_state() {
        let root = TempLib::new("definition-environments");
        std::fs::write(root.join("common.h"), "#define VALUE BASE\n#define LABEL(x) #x\n#if BASE == 2\n#define BRANCH 10\n#else\n#define BRANCH 100\n#endif\n").unwrap();
        let cache = Arc::new(DefinitionCache::default());
        let uncached = Arc::new(DefinitionCache::default());
        uncached.disable();
        for (base, branch) in [(2, "10"), (40, "100"), (2, "10")] {
            let code =
                format!("#define BASE {base}\n#include \"common.h\"\nVALUE BRANCH LABEL(hello)\n");
            let actual = tokens(&root, cache.clone(), DiagnosticSources::default(), &code).await;
            assert_eq!(actual, [base.to_string(), branch.into(), "hello".into()]);
            assert_eq!(
                actual,
                tokens(&root, uncached.clone(), DiagnosticSources::default(), &code).await
            );
        }
    }

    #[tokio::test]
    async fn edited_headers_and_cached_redefinition_diagnostics_keep_their_sources() {
        let root = TempLib::new("definition-edits");
        let cache = Arc::new(DefinitionCache::default());
        for value in [1, 2, 1] {
            std::fs::write(root.join("common.h"), format!("#define VALUE {value}\n")).unwrap();
            assert_eq!(
                tokens(
                    &root,
                    cache.clone(),
                    DiagnosticSources::default(),
                    "#include \"common.h\"\nVALUE\n"
                )
                .await,
                [value.to_string()]
            );
        }
        let compiler = CompilerBuilder::default()
            .config(temp_lib_config(&root))
            .definition_cache(cache)
            .build()
            .unwrap();
        let error = compiler
            .compile_string(
                "/main.c",
                "#include \"common.h\"\n#define VALUE 2\nint create() { return VALUE; }\n",
            )
            .await
            .unwrap_err();
        assert!(error.message().contains("duplicate `#define`"));
        assert_eq!(
            error.span().unwrap().code().as_deref(),
            Some("#define VALUE 2")
        );
    }

    #[tokio::test]
    async fn colliding_scoped_ids_do_not_reuse_another_source_storages_definition() {
        let root = TempLib::new("definition-scopes");
        let cache = Arc::new(DefinitionCache::default());
        let first = DiagnosticSources::scoped();
        let second = DiagnosticSources::scoped();
        for (sources, value) in [(first.clone(), 1), (second, 2), (first, 1)] {
            assert_eq!(
                tokens(
                    &root,
                    cache.clone(),
                    sources,
                    &format!("#define VALUE {value}\nVALUE\n")
                )
                .await,
                [value.to_string()]
            );
        }
    }

    #[derive(Debug, Default)]
    struct Gate {
        allow: AtomicBool,
        calls: AtomicUsize,
    }

    #[async_trait]
    impl CompileGate for Gate {
        async fn inherit(&self, _: &str, _: &str) -> Result<bool> {
            Ok(true)
        }
        async fn include(&self, _: &str, _: &str) -> Result<bool> {
            self.calls.fetch_add(1, Ordering::Relaxed);
            Ok(self.allow.load(Ordering::Relaxed))
        }
    }

    #[tokio::test]
    async fn cached_header_definitions_do_not_skip_include_permission() {
        let root = TempLib::new("definition-permissions");
        std::fs::write(root.join("common.h"), "#define VALUE 1\n").unwrap();
        let gate = Arc::new(Gate::default());
        gate.allow.store(true, Ordering::Relaxed);
        let compiler = CompilerBuilder::default()
            .config(temp_lib_config(&root))
            .gate(Some(gate.clone() as Arc<dyn CompileGate>))
            .build()
            .unwrap();
        let code = "#include \"common.h\"\nint create() { return VALUE; }\n";
        for _ in 0..2 {
            compiler.compile_string("/main.c", code).await.unwrap();
        }
        gate.allow.store(false, Ordering::Relaxed);
        let error = compiler.compile_string("/main.c", code).await.unwrap_err();
        assert!(error.message().contains("permission denied"));
        assert_eq!(gate.calls.load(Ordering::Relaxed), 3);
    }

    #[tokio::test]
    async fn repeated_header_preparation_reuses_cached_definitions() {
        let root = TempLib::new("definition-reuse");
        std::fs::write(
            root.join("common.h"),
            r#"#define VALUE 7
"#,
        )
        .unwrap();
        let cache = Arc::new(DefinitionCache::default());
        let sources = DiagnosticSources::default();
        let code = r#"#include "common.h"
VALUE
"#;
        assert_eq!(
            tokens(&root, cache.clone(), sources.clone(), code).await,
            ["7"]
        );
        let before = cache.stats();
        assert!(before.entries > 0);
        assert_eq!(tokens(&root, cache.clone(), sources, code).await, ["7"]);
        let after = cache.stats();
        assert!(after.hits > before.hits);
        assert_eq!(after.entries, before.entries);
    }

    #[test]
    fn function_definition_charge_covers_owned_parameter_storage() {
        use std::mem::size_of_val;

        let args: Vec<_> = (0..256)
            .map(|i| format!("p{i}").into_boxed_str().into_string())
            .collect();
        let text = format!("#define F({})", args.join(","));
        let replacement = Define::new_function(Vec::new(), args);
        let sources = DiagnosticSources::default();
        let span = Span::new(0, 0..text.len());
        let cache = DefinitionCache::default();
        cache.insert(span, text.len(), "F", &replacement, &sources);
        let stored = cache.get(span, &sources).unwrap();
        let Define::Function(function) = &stored.replacement else {
            panic!("expected a function macro");
        };
        let owned = size_of_val(function.args.as_slice())
            + function.args.iter().map(String::capacity).sum::<usize>();
        assert!(cache.stats().estimated_bytes >= owned);
    }

    #[test]
    fn object_definition_charge_covers_owned_expression_storage() {
        use crate::compiler::{lexer::LexWrapper, preprocessor::directive};
        use std::mem::size_of_val;

        fn expression_heap(expr: &PreprocessorNode) -> usize {
            match expr {
                PreprocessorNode::BinaryOp(_, left, right) => {
                    size_of_val(&**left)
                        + expression_heap(left)
                        + size_of_val(&**right)
                        + expression_heap(right)
                }
                PreprocessorNode::UnaryOp(_, inner) => {
                    size_of_val(&**inner) + expression_heap(inner)
                }
                PreprocessorNode::Var(text)
                | PreprocessorNode::String(text)
                | PreprocessorNode::Defined(text, _) => text.capacity(),
                PreprocessorNode::Int(_) => 0,
            }
        }
        let body = vec!["1"; 128].join("+");
        let text = format!("#define VALUE {body}");
        let span = Span::new(0, 0..body.len());
        let expression = directive::parse_if_expression(&body, span).unwrap();
        let tokens = LexWrapper::new(&body, 0)
            .collect::<Result<Vec<_>>>()
            .unwrap()
            .into_boxed_slice()
            .into_vec();
        let replacement = Define::new_object(tokens, Some(expression));
        let sources = DiagnosticSources::default();
        let cache = DefinitionCache::default();
        cache.insert(span, text.len(), "VALUE", &replacement, &sources);
        let stored = cache.get(span, &sources).unwrap();
        let Define::Object(object) = &stored.replacement else {
            panic!("expected an object macro");
        };
        let owned = size_of::<Definition>()
            + size_of_val(object.tokens.as_slice())
            + expression_heap(object.expr.as_ref().unwrap());
        assert!(cache.stats().estimated_bytes >= owned);
    }

    #[tokio::test]
    async fn definition_charge_depends_on_text_length_instead_of_source_offset() {
        let root = TempLib::new("definition-charge-offset");
        let mut charges = Vec::new();
        for padding in [0, 128] {
            let cache = Arc::new(DefinitionCache::default());
            let code = format!("{}#define VALUE 7\nVALUE\n", "\n".repeat(padding));

            assert_eq!(
                tokens(&root, cache.clone(), DiagnosticSources::default(), &code).await,
                ["7"]
            );
            let stats = cache.stats();
            assert_eq!(stats.entries, 1);
            charges.push(stats.estimated_bytes);
        }
        assert!(charges[0] > 0);
        assert_eq!(charges[0], charges[1]);
    }
}
