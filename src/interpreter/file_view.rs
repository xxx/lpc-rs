//! A file as the calling task will leave it: the disk, with the task's own
//! pending effects on that path applied in order, so a read sees what an
//! earlier write in the same task will commit.

use std::{
    io::{Error, ErrorKind},
    path::Path,
};

use async_trait::async_trait;

use crate::{
    compiler::source_reader::{DiskSourceReader, SourceKind, SourceReader},
    interpreter::stm::{PendingFileOp, TxnHandle},
};

/// Compiler inputs include the loading attempt's pending file changes.
#[derive(Debug)]
pub(crate) struct TransactionSourceReader(pub(crate) TxnHandle);

#[async_trait]
impl SourceReader for TransactionSourceReader {
    async fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
        read_through(&self.0, path).await?.into_bytes()
    }

    async fn kind(&self, path: &Path) -> std::io::Result<SourceKind> {
        if self.0.with(|t| t.pending_file_ops(path).is_empty()) {
            return DiskSourceReader.kind(path).await;
        }
        match read_through(&self.0, path).await? {
            Seen::File(_) => Ok(SourceKind::File),
            Seen::Dir => Ok(SourceKind::Directory),
            Seen::Missing => Err(Error::from(ErrorKind::NotFound)),
        }
    }
}

/// What is at a path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Seen {
    Missing,
    Dir,
    File(Vec<u8>),
}

impl Seen {
    /// The file's bytes, or the I/O error a read of a missing path or a
    /// directory raises.
    pub(crate) fn into_bytes(self) -> std::io::Result<Vec<u8>> {
        match self {
            Seen::File(bytes) => Ok(bytes),
            Seen::Missing => Err(Error::new(
                ErrorKind::NotFound,
                "No such file or directory (os error 2)",
            )),
            Seen::Dir => Err(Error::new(
                ErrorKind::IsADirectory,
                "Is a directory (os error 21)",
            )),
        }
    }
}

/// What is on disk at `server`.
async fn on_disk(server: &Path) -> std::io::Result<Seen> {
    match tokio::fs::metadata(server).await {
        Ok(m) if m.is_dir() => Ok(Seen::Dir),
        Ok(_) => Ok(Seen::File(tokio::fs::read(server).await?)),
        Err(e) if e.kind() == ErrorKind::NotFound => Ok(Seen::Missing),
        Err(e) => Err(e),
    }
}

/// `server` as this task will leave it: the disk with the task's
/// pending changes to the path applied. A rename's source is read as it is
/// on disk.
pub(crate) async fn read_through(txn: &TxnHandle, server: &Path) -> std::io::Result<Seen> {
    let mut seen = on_disk(server).await?;
    for op in txn.with(|t| t.pending_file_ops(server)) {
        seen = match op {
            PendingFileOp::Replace(contents) => Seen::File(contents.into_bytes()),
            PendingFileOp::Append(contents) => {
                let mut bytes = match seen {
                    Seen::File(bytes) => bytes,
                    _ => Vec::new(),
                };
                bytes.extend_from_slice(contents.as_bytes());
                Seen::File(bytes)
            }
            PendingFileOp::Remove | PendingFileOp::RemoveDir => Seen::Missing,
            PendingFileOp::MakeDir => Seen::Dir,
            PendingFileOp::CopyOf(from) => on_disk(from.server()).await?,
            PendingFileOp::WriteBytes { start, contents } => match seen {
                Seen::File(mut bytes) => {
                    let start = usize::try_from(start).unwrap_or(usize::MAX);
                    let end = start.saturating_add(contents.len());
                    if bytes.len() < end {
                        bytes.resize(end, 0);
                    }
                    bytes[start..end].copy_from_slice(&contents);
                    Seen::File(bytes)
                }
                other => other,
            },
            PendingFileOp::ReplaceChars { start, contents } => match seen {
                Seen::File(bytes) => match String::from_utf8(bytes) {
                    Ok(text) => {
                        let mut out = String::with_capacity(text.len() + contents.len());
                        let mut chars = text.chars();
                        out.extend(chars.by_ref().take(start));
                        out.push_str(&contents);
                        out.extend(chars.skip(contents.chars().count()));
                        Seen::File(out.into_bytes())
                    }
                    Err(e) => Seen::File(e.into_bytes()),
                },
                other => other,
            },
        };
    }
    Ok(seen)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            vm::{Vm, global_state::GlobalState},
        },
        test_support::{PERMISSIVE_MASTER, TempLib, temp_lib_config},
    };

    async fn allowing_vm(root: &TempLib) -> Vm {
        let config = lpc_rs_utils::config::ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/include"])
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.create_process_from_code(
            "/secure/master.c",
            format!("{PERMISSIVE_MASTER}\nint valid_write() {{ return 1; }}"),
        )
        .await
        .unwrap();
        vm
    }

    #[tokio::test]
    async fn loading_sees_new_source_headers_and_inherited_files() {
        let root = TempLib::new("compile-pending-inputs");
        let vm = allowing_vm(&root).await;
        let task = vm.initialize_process_from_code("/caller.c", indoc! {r##"
            int create() {
                mkdir("/work");
                mkdir("/include");
                write_file("/include/value.h", "#define VALUE 40\n");
                write_file("/work/local.h", "#define EXTRA 2\n");
                write_file("/work/parent.c", "#include <value.h>\nint value() { return VALUE; }");
                write_file("/work/child.c", "#include \"local.h\"\ninherit \"parent\"; int answer() { return value() + EXTRA; }");
                return "/work/child"->answer();
            }
        "##}).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(42)));
        assert!(root.join("work/child.c").is_file());
        assert!(vm.global_state.object_space.lookup("/work/child").is_some());
    }

    #[tokio::test]
    async fn replacing_source_reloads_the_new_snippet_in_the_same_attempt() {
        let root = TempLib::new("compile-replace-source");
        std::fs::write(root.join("scratch.c"), "int value() { return 1; }").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                indoc! {r##"
            int create() {
                object old = load_object("/scratch");
                int before = old->value();
                destruct(old);
                rm("/scratch.c");
                write_file("/scratch.c", "int value() { return 2; }");
                object fresh = load_object("/scratch");
                return before * 10 + fresh->value();
            }
        "##},
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(12)));
    }

    #[tokio::test]
    async fn removed_sources_and_headers_are_absent_during_compilation() {
        let root = TempLib::new("compile-removed-inputs");
        std::fs::create_dir(root.join("include")).unwrap();
        std::fs::write(root.join("value.h"), "#define VALUE 1\n").unwrap();
        std::fs::write(root.join("include/value.h"), "#define VALUE 2\n").unwrap();
        std::fs::write(root.join("gone.c"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                indoc! {r##"
            int create() {
                rm("/gone.c");
                rm("/value.h");
                write_file("/scratch.c", "#include \"value.h\"\nint value() { return VALUE; }");
                return !load_object("/gone") && "/scratch"->value() == 2;
            }
        "##},
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn an_aborted_load_publishes_neither_source_nor_object() {
        let root = TempLib::new("compile-pending-abort");
        let vm = allowing_vm(&root).await;
        let error = vm
            .initialize_process_from_code(
                "/caller.c",
                indoc! {r##"
            void create() {
                write_file("/scratch.c", "int value() { return 42; }");
                if ("/scratch"->value() != 42) throw("wrong value");
                throw("abort after execution");
            }
        "##},
            )
            .await
            .unwrap_err();
        assert!(
            error.to_string().contains("abort after execution"),
            "{error}"
        );
        assert!(!root.join("scratch.c").exists());
        assert!(vm.global_state.object_space.lookup("/scratch").is_none());
    }

    #[tokio::test]
    async fn rejected_attempts_recompile_without_duplicate_file_effects() {
        let root = TempLib::new("compile-pending-retry");
        let (tx, _rx) = tokio::sync::mpsc::channel(16);
        let state = Arc::new(GlobalState::new_rejecting(
            Arc::new(temp_lib_config(&root)),
            tx,
            3,
        ));
        state
            .object_space
            .create_process_from_code(
                "/secure/master.c",
                format!("{PERMISSIVE_MASTER}\nint valid_write() {{ return 1; }}"),
            )
            .await
            .unwrap();
        let task = state
            .initialize_process_from_code(
                "/caller.c",
                indoc! {r##"
            int create() {
                write_file("/scratch.c", "int value() { return 42; }");
                return "/scratch"->value();
            }
        "##},
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(42)));
        assert_eq!(state.attempt_telemetry().conflicts, 3);
        assert_eq!(
            std::fs::read_to_string(root.join("scratch.c")).unwrap(),
            "int value() { return 42; }"
        );
        assert!(state.object_space.lookup("/scratch").is_some());
    }
}
