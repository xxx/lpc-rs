//! The master's say over what a compile may read for another file: a parent
//! to inherit, a file to include.

use std::fmt::Debug;

use async_trait::async_trait;
use lpc_rs_errors::Result;

/// Consulted before a compile reads a file on another file's behalf. A
/// compiler without one (boot, lpcc, direct compiles) reads freely.
#[async_trait]
pub trait CompileGate: Debug + Send + Sync {
    /// May the program `from` inherit `path`? Both are canonical in-game
    /// `.c` paths.
    async fn inherit(&self, path: &str, from: &str) -> Result<bool>;

    /// May the file `from` include `path`? Both are canonical in-game paths.
    async fn include(&self, path: &str, from: &str) -> Result<bool>;
}

#[cfg(test)]
pub(crate) mod test_gate {
    use std::sync::Mutex;

    use super::*;

    /// Records every question as `(path, from)` and answers `allow`.
    #[derive(Debug, Default)]
    pub(crate) struct RecordingGate {
        pub allow: bool,
        pub inherits: Mutex<Vec<(String, String)>>,
        pub includes: Mutex<Vec<(String, String)>>,
    }

    impl RecordingGate {
        pub(crate) fn allowing() -> Self {
            Self {
                allow: true,
                ..Self::default()
            }
        }

        pub(crate) fn denying() -> Self {
            Self::default()
        }

        pub(crate) fn inherits(&self) -> Vec<(String, String)> {
            self.inherits.lock().unwrap().clone()
        }

        pub(crate) fn includes(&self) -> Vec<(String, String)> {
            self.includes.lock().unwrap().clone()
        }
    }

    #[async_trait]
    impl CompileGate for RecordingGate {
        async fn inherit(&self, path: &str, from: &str) -> Result<bool> {
            self.inherits
                .lock()
                .unwrap()
                .push((path.into(), from.into()));
            Ok(self.allow)
        }

        async fn include(&self, path: &str, from: &str) -> Result<bool> {
            self.includes
                .lock()
                .unwrap()
                .push((path.into(), from.into()));
            Ok(self.allow)
        }
    }
}

#[cfg(test)]
mod gate_tests {
    use std::sync::Arc;

    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::{Config, ConfigBuilder};

    use super::{CompileGate, test_gate::RecordingGate};
    use crate::{
        compiler::{Compiled, CompilerBuilder},
        test_support::TempLib,
    };

    fn write(root: &TempLib, rel: &str, text: &str) {
        let path = root.join(rel);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }

    fn config_at(root: &TempLib) -> Arc<Config> {
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .build()
            .unwrap()
            .into()
    }

    /// Compile `/child` under `gate`; the result and the gate's record.
    async fn compile_under(
        config: Arc<Config>,
        gate: RecordingGate,
    ) -> (lpc_rs_errors::Result<Compiled>, Arc<RecordingGate>) {
        let gate = Arc::new(gate);
        let dyn_gate: Arc<dyn CompileGate> = gate.clone();
        let compiler = CompilerBuilder::default()
            .config(config.clone())
            .gate(Some(dyn_gate))
            .build()
            .unwrap();
        let path = LpcPath::new_in_game("/child", "/", &*config.lib_dir);
        (compiler.compile_in_game_file(&path, None).await, gate)
    }

    fn pairs(v: &[(&str, &str)]) -> Vec<(String, String)> {
        v.iter()
            .map(|(a, b)| (a.to_string(), b.to_string()))
            .collect()
    }

    #[tokio::test]
    async fn an_inherit_asks_with_the_program_as_from() {
        let root = TempLib::new("gate-inherit");
        write(&root, "parent.c", "int p;\n");
        write(&root, "child.c", "inherit \"/parent\";\n");
        let (result, gate) = compile_under(config_at(&root), RecordingGate::allowing()).await;
        result.unwrap();
        assert_eq!(gate.inherits(), pairs(&[("/parent.c", "/child.c")]));
    }

    /// The inherit line lives in a header; the program still gains the parent.
    #[tokio::test]
    async fn an_inherit_written_in_a_header_names_the_program() {
        let root = TempLib::new("gate-inherit-header");
        write(&root, "parent.c", "int p;\n");
        write(&root, "hdr.h", "inherit \"/parent\";\n");
        write(&root, "child.c", "#include \"/hdr.h\"\n");
        let (result, gate) = compile_under(config_at(&root), RecordingGate::allowing()).await;
        result.unwrap();
        assert_eq!(gate.inherits(), pairs(&[("/parent.c", "/child.c")]));
        assert_eq!(gate.includes(), pairs(&[("/hdr.h", "/child.c")]));
    }

    /// Each compile asks for its own inherits: the parent asks for the grandparent.
    #[tokio::test]
    async fn a_nested_inherit_asks_from_the_parent() {
        let root = TempLib::new("gate-inherit-nested");
        write(&root, "grandparent.c", "int g;\n");
        write(&root, "parent.c", "inherit \"/grandparent\";\n");
        write(&root, "child.c", "inherit \"/parent\";\n");
        let (result, gate) = compile_under(config_at(&root), RecordingGate::allowing()).await;
        result.unwrap();
        assert_eq!(
            gate.inherits(),
            pairs(&[("/parent.c", "/child.c"), ("/grandparent.c", "/parent.c")])
        );
    }

    #[tokio::test]
    async fn a_nested_include_asks_from_the_header() {
        let root = TempLib::new("gate-include-nested");
        write(&root, "a.h", "#include \"b.h\"\n");
        write(&root, "b.h", "int b;\n");
        write(&root, "child.c", "#include \"/a.h\"\n");
        let (result, gate) = compile_under(config_at(&root), RecordingGate::allowing()).await;
        result.unwrap();
        assert_eq!(
            gate.includes(),
            pairs(&[("/a.h", "/child.c"), ("/b.h", "/a.h")])
        );
    }

    /// Every directive is authorized, memo and `#pragma once` notwithstanding.
    #[tokio::test]
    async fn a_repeated_include_asks_each_time() {
        let root = TempLib::new("gate-include-repeat");
        write(&root, "o.h", "#pragma once\nint o;\n");
        write(&root, "child.c", "#include \"/o.h\"\n#include \"/o.h\"\n");
        let (result, gate) = compile_under(config_at(&root), RecordingGate::allowing()).await;
        result.unwrap();
        assert_eq!(gate.includes().len(), 2);
    }

    #[tokio::test]
    async fn the_configured_auto_files_are_not_asked() {
        let root = TempLib::new("gate-auto");
        write(&root, "auto.h", "#define AUTO 1\n");
        write(&root, "auto.c", "int auto_var;\n");
        write(&root, "child.c", "int c = AUTO;\n");
        let config: Arc<Config> = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .auto_include_file("/auto.h")
            .auto_inherit_file("/auto.c")
            .build()
            .unwrap()
            .into();
        let (result, gate) = compile_under(config, RecordingGate::denying()).await;
        result.unwrap();
        assert!(gate.inherits().is_empty());
        assert!(gate.includes().is_empty());
    }

    #[tokio::test]
    async fn a_denied_inherit_is_a_compile_error_at_the_directive() {
        let root = TempLib::new("gate-inherit-deny");
        write(&root, "parent.c", "int p;\n");
        // The grammar takes every inherit before any definition.
        write(&root, "child.c", "// a line before\ninherit \"/parent\";\n");
        let (result, _) = compile_under(config_at(&root), RecordingGate::denying()).await;
        let e = result.map(|_| ()).unwrap_err();
        assert_eq!(e.to_string(), "inherit \"/parent.c\": permission denied");
        assert!(e.span().is_some(), "the directive's span");
    }

    /// Denied before the read: the file's contents never enter the compile.
    #[tokio::test]
    async fn a_denied_include_is_a_compile_error_at_the_directive() {
        let root = TempLib::new("gate-include-deny");
        write(&root, "child.c", "#include \"/secret.h\"\n");
        let (result, _) = compile_under(config_at(&root), RecordingGate::denying()).await;
        let e = result.map(|_| ()).unwrap_err();
        assert_eq!(e.to_string(), "#include \"/secret.h\": permission denied");
        assert!(e.span().is_some(), "the directive's span");
    }

    /// An escaping parent path is a confinement error before the gate hears of it.
    #[tokio::test]
    async fn an_escaping_inherit_is_refused_without_asking() {
        let root = TempLib::new("gate-inherit-escape");
        write(
            &root,
            "child.c",
            "inherit \"/../../../../../../../../etc/passwd\";\n",
        );
        let (result, gate) = compile_under(config_at(&root), RecordingGate::allowing()).await;
        let e = result.map(|_| ()).unwrap_err().to_string();
        assert_eq!(
            e,
            "attempt to inherit a file outside the root: \
             `/../../../../../../../../etc/passwd`"
        );
        assert!(gate.inherits().is_empty());
    }

    #[tokio::test]
    async fn without_a_gate_nothing_is_asked_and_everything_compiles() {
        let root = TempLib::new("gate-none");
        write(&root, "parent.c", "int p;\n");
        write(&root, "h.h", "int h;\n");
        write(
            &root,
            "child.c",
            "inherit \"/parent\";\n#include \"/h.h\"\n",
        );
        let config = config_at(&root);
        let compiler = CompilerBuilder::default()
            .config(config.clone())
            .build()
            .unwrap();
        compiler
            .compile_in_game_file(&LpcPath::new_in_game("/child", "/", &*config.lib_dir), None)
            .await
            .unwrap();
    }
}
