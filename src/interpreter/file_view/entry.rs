//! Entry metadata for unlinking, without following leaf symlinks or reading contents.

use std::{
    io::{self, ErrorKind},
    path::{Path, PathBuf},
};

use indexmap::IndexSet;

use crate::interpreter::stm::{Effect, TxnHandle};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntryKind {
    Missing,
    File,
    Symlink,
    Directory,
    Other,
}

#[derive(Debug)]
enum EntryOp {
    Append(usize),
    Replace(usize),
    RemoveFile(usize),
    MakeDir(usize),
    RemoveDir(usize),
    Rename { from: usize, to: usize },
}

/// Only paths and effect tags escape the transaction lock; file payloads stay borrowed.
#[derive(Debug, Default)]
pub(crate) struct PendingEntryView {
    paths: Vec<PathBuf>,
    ops: Vec<EntryOp>,
}

impl PendingEntryView {
    pub(crate) fn new(effects: &[Effect], server: &Path) -> Self {
        if !effects.iter().any(|effect| effect.affects_file(server)) {
            return Self::default();
        }

        let mut paths = IndexSet::from([server]);
        let mut ops = Vec::new();
        // Walking backwards collects each rename's dependencies before replaying them forwards.
        for effect in effects.iter().rev() {
            let op = match effect {
                Effect::AppendFile { path, .. } => {
                    paths.get_index_of(path.server()).map(EntryOp::Append)
                }
                Effect::WriteFile { path, .. } => {
                    paths.get_index_of(path.server()).map(EntryOp::Replace)
                }
                Effect::RemoveFile { path } => {
                    paths.get_index_of(path.server()).map(EntryOp::RemoveFile)
                }
                Effect::CreateDir { path } => {
                    paths.get_index_of(path.server()).map(EntryOp::MakeDir)
                }
                Effect::RemoveDir { path } => {
                    paths.get_index_of(path.server()).map(EntryOp::RemoveDir)
                }
                Effect::Rename { path, to }
                    if path.server() != to.server()
                        && (paths.contains(path.server()) || paths.contains(to.server())) =>
                {
                    let (from, _) = paths.insert_full(path.server());
                    let (to, _) = paths.insert_full(to.server());
                    Some(EntryOp::Rename { from, to })
                }
                _ => None,
            };
            if let Some(op) = op {
                ops.push(op);
            }
        }
        ops.reverse();
        Self {
            paths: paths.into_iter().map(Path::to_path_buf).collect(),
            ops,
        }
    }

    async fn kind(self, server: &Path) -> io::Result<EntryKind> {
        if self.ops.is_empty() {
            return on_disk(server).await;
        }
        let mut kinds = Vec::with_capacity(self.paths.len());
        for path in &self.paths {
            kinds.push(on_disk(path).await?);
        }
        use EntryKind::{Directory, File, Missing, Symlink};
        for op in self.ops {
            match op {
                EntryOp::Append(i) if kinds[i] == Missing => kinds[i] = File,
                EntryOp::Replace(i) if kinds[i] != Directory => kinds[i] = File,
                EntryOp::RemoveFile(i) if matches!(kinds[i], File | Symlink) => {
                    kinds[i] = Missing;
                }
                EntryOp::MakeDir(i) if kinds[i] == Missing => kinds[i] = Directory,
                EntryOp::RemoveDir(i) if kinds[i] == Directory => kinds[i] = Missing,
                EntryOp::Rename { from, to }
                    if kinds[from] != Missing
                        && (kinds[to] == Missing
                            || (kinds[from] == Directory) == (kinds[to] == Directory)) =>
                {
                    kinds[to] = kinds[from];
                    kinds[from] = Missing;
                }
                _ => {}
            }
        }
        Ok(kinds[0])
    }
}

async fn on_disk(server: &Path) -> io::Result<EntryKind> {
    match tokio::fs::symlink_metadata(server).await {
        Ok(metadata) if metadata.is_file() => Ok(EntryKind::File),
        Ok(metadata) if metadata.is_symlink() => Ok(EntryKind::Symlink),
        Ok(metadata) if metadata.is_dir() => Ok(EntryKind::Directory),
        Ok(_) => Ok(EntryKind::Other),
        Err(error) if error.kind() == ErrorKind::NotFound => Ok(EntryKind::Missing),
        Err(error) => Err(error),
    }
}

/// Apply this attempt's exact-path entry changes, including rename history.
pub(crate) async fn entry_kind(txn: &TxnHandle, server: &Path) -> io::Result<EntryKind> {
    txn.with(|t| t.pending_entry_view(server))
        .kind(server)
        .await
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LibRoot;

    use super::*;
    use crate::test_support::TempLib;

    #[cfg(unix)]
    #[tokio::test]
    async fn rename_chains_use_historical_source_kinds() {
        let root = TempLib::new("entry-rename-history");
        let lib = LibRoot::new(&*root);
        std::os::unix::fs::symlink("missing", root.join("a")).unwrap();
        std::fs::write(root.join("b"), "file").unwrap();
        let txn = TxnHandle::empty();
        txn.with(|t| {
            t.record_effect(Effect::Rename {
                path: lib.resolve("/a", "/").unwrap(),
                to: lib.resolve("/b", "/").unwrap(),
            });
            t.record_effect(Effect::Rename {
                path: lib.resolve("/b", "/").unwrap(),
                to: lib.resolve("/c", "/").unwrap(),
            });
            t.record_effect(Effect::CreateDir {
                path: lib.resolve("/a", "/").unwrap(),
            });
        });
        assert_eq!(
            entry_kind(&txn, &root.join("c")).await.unwrap(),
            EntryKind::Symlink
        );
        assert_eq!(
            entry_kind(&txn, &root.join("a")).await.unwrap(),
            EntryKind::Directory
        );
        assert_eq!(
            entry_kind(&txn, &root.join("b")).await.unwrap(),
            EntryKind::Missing
        );
    }

    #[tokio::test]
    async fn an_incompatible_rename_preserves_both_entry_kinds() {
        let root = TempLib::new("entry-incompatible-rename");
        let lib = LibRoot::new(&*root);
        std::fs::create_dir(root.join("dir")).unwrap();
        std::fs::write(root.join("file"), "file").unwrap();
        for (from, to) in [("/dir", "/file"), ("/file", "/dir")] {
            let txn = TxnHandle::empty();
            txn.with(|t| {
                t.record_effect(Effect::Rename {
                    path: lib.resolve(from, "/").unwrap(),
                    to: lib.resolve(to, "/").unwrap(),
                })
            });
            assert_eq!(
                entry_kind(&txn, &root.join("dir")).await.unwrap(),
                EntryKind::Directory
            );
            assert_eq!(
                entry_kind(&txn, &root.join("file")).await.unwrap(),
                EntryKind::File
            );
        }
    }

    #[tokio::test]
    async fn a_failed_save_does_not_reclassify_a_directory_as_a_file() {
        let root = TempLib::new("entry-save-directory");
        let lib = LibRoot::new(&*root);
        std::fs::create_dir(root.join("dir.o")).unwrap();
        let txn = TxnHandle::empty();
        txn.with(|t| {
            t.record_effect(Effect::WriteFile {
                path: lib.resolve("/dir.o", "/").unwrap(),
                contents: "x 1\n".into(),
            })
        });
        assert_eq!(
            entry_kind(&txn, &root.join("dir.o")).await.unwrap(),
            EntryKind::Directory
        );
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn append_preserves_a_symlink_and_save_replaces_it() {
        let root = TempLib::new("entry-write-symlink");
        let lib = LibRoot::new(&*root);
        std::os::unix::fs::symlink("missing", root.join("link")).unwrap();
        let txn = TxnHandle::empty();
        txn.with(|t| {
            t.record_effect(Effect::AppendFile {
                path: lib.resolve("/link", "/").unwrap(),
                contents: "appended".into(),
            })
        });
        assert_eq!(
            entry_kind(&txn, &root.join("link")).await.unwrap(),
            EntryKind::Symlink
        );
        txn.with(|t| {
            t.record_effect(Effect::WriteFile {
                path: lib.resolve("/link", "/").unwrap(),
                contents: "replacement".into(),
            })
        });
        assert_eq!(
            entry_kind(&txn, &root.join("link")).await.unwrap(),
            EntryKind::File
        );
        assert!(
            std::fs::symlink_metadata(root.join("link"))
                .unwrap()
                .is_symlink()
        );
    }

    #[tokio::test]
    async fn in_place_writes_do_not_create_missing_entries() {
        let root = TempLib::new("entry-inplace-missing");
        let lib = LibRoot::new(&*root);
        let txn = TxnHandle::empty();
        txn.with(|t| {
            t.record_effect(Effect::WriteBytes {
                path: lib.resolve("/file", "/").unwrap(),
                start: 0,
                contents: vec![42],
            });
            t.record_effect(Effect::ReplaceChars {
                path: lib.resolve("/file", "/").unwrap(),
                start: 0,
                contents: "text".into(),
            });
        });
        assert_eq!(
            entry_kind(&txn, &root.join("file")).await.unwrap(),
            EntryKind::Missing
        );
    }

    #[tokio::test]
    async fn unrelated_paths_are_not_inspected_when_replaying_a_rename() {
        let root = TempLib::new("entry-unrelated-error");
        let lib = LibRoot::new(&*root);
        std::fs::write(root.join("file"), "file").unwrap();
        let txn = TxnHandle::empty();
        txn.with(|t| {
            t.record_effect(Effect::WriteFile {
                path: lib.resolve("/file/not-a-directory", "/").unwrap(),
                contents: "cannot write".into(),
            });
            t.record_effect(Effect::Rename {
                path: lib.resolve("/file", "/").unwrap(),
                to: lib.resolve("/moved", "/").unwrap(),
            });
        });
        assert_eq!(
            entry_kind(&txn, &root.join("moved")).await.unwrap(),
            EntryKind::File
        );
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn special_entries_are_classified_without_opening_them() {
        let root = TempLib::new("entry-socket");
        let lib = LibRoot::new(&*root);
        let _socket = std::os::unix::net::UnixListener::bind(root.join("socket")).unwrap();
        let txn = TxnHandle::empty();
        txn.with(|t| {
            t.record_effect(Effect::AppendFile {
                path: lib.resolve("/socket", "/").unwrap(),
                contents: "not writable".into(),
            })
        });
        assert_eq!(
            entry_kind(&txn, &root.join("socket")).await.unwrap(),
            EntryKind::Other
        );
    }
}
