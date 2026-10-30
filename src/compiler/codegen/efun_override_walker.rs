//! Authorize explicit efun calls before later passes transform expressions.

use async_trait::async_trait;
use lpc_rs_core::{EFUN, call_namespace::CallNamespace};
use lpc_rs_errors::{Result, lpc_bug, lpc_error};

use super::tree_walker::{ContextHolder, Pass, TreeWalker, walk_call_root};
use crate::{
    compiler::{
        ast::call_node::{CallChain, CallNode},
        compilation_context::CompilationContext,
        diagnostics::Diagnostics,
    },
    interpreter::efun::EFUN_PROTOTYPES,
};

pub(crate) struct EfunOverrideWalker {
    context: CompilationContext,
}

impl ContextHolder for EfunOverrideWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl Pass for EfunOverrideWalker {
    fn new(context: CompilationContext) -> Self {
        Self { context }
    }

    fn diagnostics_mut(&mut self) -> &mut Diagnostics {
        &mut self.context.diagnostics
    }
}

#[async_trait]
impl TreeWalker for EfunOverrideWalker {
    async fn visit_call_root(&mut self, node: &mut CallNode) -> Result<()> {
        if let CallChain::Root {
            receiver: None,
            namespace: CallNamespace::Named(namespace),
            name,
        } = &node.chain
            && namespace.as_str() == EFUN
            && EFUN_PROTOTYPES.contains_key(name.as_str())
            && let Some(gate) = &self.context.gate
        {
            let source = node.span.and_then(|span| {
                self.context
                    .diagnostic_sources
                    .read()
                    .get(span.file_id())
                    .ok()
                    .map(|file| file.name().clone())
            });
            let source = source.ok_or_else(|| {
                self.context.diagnostics.fail(lpc_bug!(
                    node.span,
                    "explicit efun call has no source identity"
                ))
            })?;
            let allowed = gate
                .efun_override(&source, name, self.context.source.name().as_str())
                .await
                .map_err(|error| self.context.diagnostics.fail(error.or_span(node.span)))?;
            if !allowed {
                return Err(self.context.diagnostics.fail(lpc_error!(
                    node.span,
                    "efun::{}: permission denied",
                    name
                )));
            }
        }
        walk_call_root(self, node).await
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    };

    use lpc_rs_errors::source_map::DiagnosticSources;
    use lpc_rs_utils::config::ConfigBuilder;
    use parking_lot::Mutex;

    use super::*;
    use crate::{
        compiler::{Compiler, CompilerBuilder, compile_gate::CompileGate},
        test_support::TempLib,
    };

    #[derive(Debug, Default)]
    struct Gate {
        allow: AtomicBool,
        calls: Mutex<Vec<(String, String, String)>>,
    }

    #[async_trait]
    impl CompileGate for Gate {
        async fn inherit(&self, _: &str, _: &str) -> Result<bool> {
            Ok(true)
        }
        async fn include(&self, _: &str, _: &str) -> Result<bool> {
            Ok(true)
        }
        async fn efun_override(&self, source: &str, name: &str, main: &str) -> Result<bool> {
            self.calls
                .lock()
                .push((source.into(), name.into(), main.into()));
            Ok(self.allow.load(Ordering::Relaxed))
        }
    }

    fn compiler(root: &TempLib, gate: &Arc<Gate>, sources: DiagnosticSources) -> Compiler {
        CompilerBuilder::default()
            .config(
                ConfigBuilder::default()
                    .lib_dir(root.to_str().unwrap())
                    .build()
                    .unwrap(),
            )
            .gate(Some(gate.clone() as Arc<dyn CompileGate>))
            .diagnostic_sources(sources)
            .build()
            .unwrap()
    }

    #[tokio::test]
    async fn explicit_efuns_are_checked_in_nested_expression_positions() {
        let root = TempLib::new("override-expressions");
        let gate = Arc::new(Gate::default());
        let compiler = compiler(&root, &gate, DiagnosticSources::scoped());
        for code in [
            "int run() { return efun::sizeof(({ 1 })); }",
            "function run() { return (: efun::sizeof(({ 1 })) :); }",
            "mixed run() { return this_object()->take(efun::sizeof(({ 1 }))); }",
            "mixed run() { return efun::this_object()->take(); }",
            "function run() { return (: $(efun::sizeof(({ 1 }))) :); }",
            "int run(int n=efun::sizeof(({ 1 }))) { return n; }",
            "int n=efun::sizeof(({ 1 }));",
            "int run() { return (1, efun::sizeof(({ 1 })), 2); }",
            "int run() { if (0) return efun::sizeof(({ 1 })); return 1; }",
            "class item { int value; } mixed run() { return new(class item, value: efun::sizeof(({ 1 }))); }",
        ] {
            let error = compiler
                .compile_string("/caller.c", code)
                .await
                .unwrap_err();
            assert!(
                error.to_string().contains("permission denied"),
                "{code}: {error}"
            );
            assert!(error.span().is_some());
        }
        assert_eq!(gate.calls.lock().len(), 10);
    }

    #[tokio::test]
    async fn headers_and_macros_report_canonical_source_and_main_file() {
        for sources in [DiagnosticSources::default(), DiagnosticSources::scoped()] {
            let root = TempLib::new("override-source");
            std::fs::create_dir_all(root.join("headers")).unwrap();
            std::fs::write(root.join("headers/macros.h"),
                "#define RAW efun::sizeof\n#define WRAP(x) x\nint header() { return efun::sizeof(({ 1 })); }\n").unwrap();
            std::fs::write(
                root.join("outer.h"),
                "#include \"headers/macros.h\"\nint nested() { return RAW(({ 2 })); }\n",
            )
            .unwrap();
            let gate = Arc::new(Gate::default());
            gate.allow.store(true, Ordering::Relaxed);
            compiler(&root, &gate, sources)
                .compile_string(
                    "/dir/../main.c",
                    r#"
                #include "/outer.h"
                int run() { return RAW(({ 3 })) + WRAP(efun::sizeof(({ 4 }))); }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(
                *gate.calls.lock(),
                [
                    (
                        "/headers/macros.h".into(),
                        "sizeof".into(),
                        "/main.c".into()
                    ),
                    ("/outer.h".into(), "sizeof".into(), "/main.c".into()),
                    ("/main.c".into(), "sizeof".into(), "/main.c".into()),
                    ("/main.c".into(), "sizeof".into(), "/main.c".into()),
                ]
            );
        }
    }

    #[tokio::test]
    async fn denied_header_calls_keep_the_header_diagnostic_and_encoding_warning() {
        let root = TempLib::new("override-header-denied");
        std::fs::write(
            root.join("header.h"),
            b"// \xe9\nint f() { return efun::sizeof(({ 1 })); }",
        )
        .unwrap();
        let gate = Arc::new(Gate::default());
        let sources = DiagnosticSources::scoped();
        let error = compiler(&root, &gate, sources.clone())
            .compile_string("/main.c", "#include \"/header.h\"\n")
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "efun::sizeof: permission denied");
        let diagnostics = error.to_diagnostics();
        assert_eq!(diagnostics.len(), 2);
        assert!(diagnostics[1].message.contains("read as Latin-1"));
        assert_eq!(
            sources
                .read()
                .get(error.span().unwrap().file_id())
                .unwrap()
                .name(),
            "/header.h"
        );
    }

    #[tokio::test]
    async fn inherited_shared_code_is_authorized_on_each_compilation() {
        let root = TempLib::new("override-shared");
        std::fs::write(
            root.join("parent.c"),
            "int f() { return efun::sizeof(({ 1 })); }",
        )
        .unwrap();
        let gate = Arc::new(Gate::default());
        gate.allow.store(true, Ordering::Relaxed);
        let compiler = compiler(&root, &gate, DiagnosticSources::scoped());
        let first = compiler
            .compile_string("/first.c", "inherit \"/parent\";")
            .await
            .unwrap();
        let second = compiler
            .compile_string("/second.c", "inherit \"/parent\";")
            .await
            .unwrap();
        assert!(Arc::ptr_eq(
            &first.program.lookup_function("f").unwrap().code,
            &second.program.lookup_function("f").unwrap().code
        ));
        gate.allow.store(false, Ordering::Relaxed);
        let error = compiler
            .compile_string("/denied.c", "inherit \"/parent\";")
            .await
            .unwrap_err();
        assert!(error.to_string().contains("permission denied"));
        assert_eq!(
            *gate.calls.lock(),
            vec![("/parent.c".into(), "sizeof".into(), "/parent.c".into()); 3]
        );
    }

    #[tokio::test]
    async fn configured_sources_still_authorize_explicit_efuns() {
        for (include, inherit) in [(true, false), (false, true)] {
            let root = TempLib::new("override-auto");
            std::fs::write(
                root.join("auto.h"),
                "int h() { return efun::sizeof(({ 1 })); }",
            )
            .unwrap();
            std::fs::write(
                root.join("auto.c"),
                "int p() { return efun::sizeof(({ 1 })); }",
            )
            .unwrap();
            let mut config = ConfigBuilder::default().lib_dir(root.to_str().unwrap());
            if include {
                config = config.auto_include_file("/auto.h");
            }
            if inherit {
                config = config.auto_inherit_file("/auto.c");
            }
            let gate = Arc::new(Gate::default());
            let compiler = CompilerBuilder::default()
                .config(config.build().unwrap())
                .gate(Some(gate.clone() as Arc<dyn CompileGate>))
                .build()
                .unwrap();
            assert!(
                compiler
                    .compile_string("/main.c", "int n;")
                    .await
                    .unwrap_err()
                    .to_string()
                    .contains("permission denied")
            );
            let calls = gate.calls.lock();
            assert_eq!(calls.len(), 1);
            assert_eq!(calls[0].0, if include { "/auto.h" } else { "/auto.c" });
            assert_eq!(calls[0].2, if include { "/main.c" } else { "/auto.c" });
        }
    }

    #[tokio::test]
    async fn bare_calls_inactive_source_and_unknown_efuns_do_not_ask() {
        let root = TempLib::new("override-bare");
        let gate = Arc::new(Gate::default());
        let compiler = compiler(&root, &gate, DiagnosticSources::scoped());
        compiler
            .compile_string(
                "/bare.c",
                r#"
            #if 0
            int bad() { return efun::sizeof(({ 1 })); }
            #endif
            int run() { function f=sizeof; return sizeof(({ 1 }))+f(({ 2 })); }
        "#,
            )
            .await
            .unwrap();
        let error = compiler
            .compile_string("/unknown.c", "int f() { return efun::not_an_efun(); }")
            .await
            .unwrap_err();
        assert!(error.to_string().contains("unknown function"));
        assert!(gate.calls.lock().is_empty());
    }
}
