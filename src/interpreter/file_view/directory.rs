//! Directory membership after this attempt's pending entry operations.

use std::{
    ffi::OsString,
    io::{self, ErrorKind},
    path::Path,
};

use indexmap::IndexSet;

use super::entry::{EntryKind, EntryOp, PendingEntryView, on_disk};
use crate::interpreter::stm::{Effect, TxnHandle};

/// A metadata-only snapshot of directory changes and their rename dependencies.
#[derive(Debug)]
pub(crate) struct PendingDirectoryView {
    entries: PendingEntryView,
    directories: Vec<usize>,
}

impl PendingDirectoryView {
    pub(crate) fn new(effects: &[Effect], server: &Path) -> Self {
        let mut directories = IndexSet::from([server]);
        loop {
            let old_len = directories.len();
            for effect in effects {
                if let Effect::Rename { path, to } = effect {
                    let touches = |p: &Path| {
                        directories.contains(p)
                            || p.parent().is_some_and(|p| directories.contains(p))
                    };
                    if touches(path.server()) || touches(to.server()) {
                        directories.insert(path.server());
                        directories.insert(to.server());
                    }
                }
            }
            if directories.len() == old_len {
                break;
            }
        }
        let mut paths = directories.clone();
        for effect in effects {
            let path = match effect {
                Effect::AppendFile { path, .. }
                | Effect::WriteFile { path, .. }
                | Effect::RemoveFile { path }
                | Effect::CreateDir { path }
                | Effect::RemoveDir { path } => path.server(),
                _ => continue,
            };
            if path.parent().is_some_and(|p| directories.contains(p)) {
                paths.insert(path);
            }
        }
        Self {
            directories: (0..directories.len()).collect(),
            entries: PendingEntryView::collect(effects, paths),
        }
    }

    async fn check_empty(self) -> io::Result<bool> {
        if self.entries.ops.is_empty() {
            return match on_disk(&self.entries.paths[0]).await? {
                EntryKind::Directory => Ok(tokio::fs::read_dir(&self.entries.paths[0])
                    .await?
                    .next_entry()
                    .await?
                    .is_none()),
                EntryKind::Missing => Err(missing_directory()),
                _ => Err(ErrorKind::NotADirectory.into()),
            };
        }
        let mut kinds = Vec::with_capacity(self.entries.paths.len());
        for path in &self.entries.paths {
            kinds.push(on_disk(path).await?);
        }
        let mut children: Vec<IndexSet<OsString>> = (0..self.directories.len())
            .map(|_| IndexSet::new())
            .collect();
        for &i in &self.directories {
            if kinds[i] == EntryKind::Directory {
                let mut entries = tokio::fs::read_dir(&self.entries.paths[i]).await?;
                while let Some(entry) = entries.next_entry().await? {
                    children[i].insert(entry.file_name());
                }
            }
        }
        for op in &self.entries.ops {
            if let EntryOp::Rename { from, to } = *op
                && kinds[from] == EntryKind::Directory
                && children.get(to).is_some_and(|c| !c.is_empty())
            {
                continue;
            }
            if !op.apply(&mut kinds) {
                continue;
            }
            match *op {
                EntryOp::MakeDir(i) | EntryOp::RemoveDir(i) => {
                    if let Some(contents) = children.get_mut(i) {
                        contents.clear();
                    }
                }
                EntryOp::Rename { from, to } => {
                    let contents = std::mem::take(&mut children[from]);
                    children[to] = contents;
                }
                _ => {}
            }
            let (first, second) = op.indices();
            for i in std::iter::once(first).chain(second) {
                let path = &self.entries.paths[i];
                for &directory in &self.directories {
                    if path.parent() == Some(self.entries.paths[directory].as_path())
                        && let Some(name) = path.file_name()
                    {
                        if kinds[i] == EntryKind::Missing {
                            children[directory].shift_remove(name);
                        } else {
                            children[directory].insert(name.to_os_string());
                        }
                    }
                }
            }
        }
        match kinds[0] {
            EntryKind::Directory => Ok(children[0].is_empty()),
            EntryKind::Missing => Err(missing_directory()),
            _ => Err(ErrorKind::NotADirectory.into()),
        }
    }
}

fn missing_directory() -> io::Error {
    io::Error::new(
        ErrorKind::NotFound,
        "No such file or directory (os error 2)",
    )
}

/// Test directory emptiness after earlier effects, without reading file contents.
pub(crate) async fn directory_is_empty(txn: &TxnHandle, server: &Path) -> io::Result<bool> {
    txn.with(|t| t.pending_directory_view(server))
        .check_empty()
        .await
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::TempLib;
    use lpc_rs_core::lpc_path::LibRoot;

    #[tokio::test]
    async fn recreated_directory_views_do_not_keep_old_disk_membership() {
        let root = TempLib::new("recreated-directory-view");
        let server = root.join("dir");
        std::fs::create_dir(&server).unwrap();
        std::fs::write(server.join("old"), "old content").unwrap();
        let lib = LibRoot::new(&*root);
        let effects = [
            Effect::RemoveDir {
                path: lib.resolve("/dir", "/").unwrap(),
            },
            Effect::CreateDir {
                path: lib.resolve("/dir", "/").unwrap(),
            },
        ];
        let view = PendingDirectoryView::new(&effects, &server);
        assert!(view.check_empty().await.unwrap());
        assert!(server.join("old").exists());
    }
}
