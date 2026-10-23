//! File kind and byte length after the calling task's pending effects.

use std::{io, path::Path};

use crate::interpreter::stm::{Effect, TxnHandle};

use super::{Seen, read_through};

#[derive(Debug, Clone, Copy)]
pub(crate) enum FileMetadata {
    Missing,
    Directory,
    File(u64),
    Other(u64),
}

impl FileMetadata {
    /// Apply size-only effects without copying their payloads.
    pub(crate) fn after_effects(self, effects: &[Effect], server: &Path) -> Option<Self> {
        let mut seen = self;
        for effect in effects.iter().filter(|effect| effect.affects_file(server)) {
            seen = match effect {
                Effect::WriteFile { contents, .. } => Self::File(contents.len() as u64),
                Effect::AppendFile { contents, .. } => {
                    let size = match seen {
                        Self::File(size) => size,
                        Self::Other(_) => return None,
                        _ => 0,
                    };
                    Self::File(size.saturating_add(contents.len() as u64))
                }
                Effect::WriteBytes {
                    start, contents, ..
                } => match seen {
                    Self::File(size) => {
                        Self::File(size.max(start.saturating_add(contents.len() as u64)))
                    }
                    other => other,
                },
                Effect::RemoveFile { .. } | Effect::RemoveDir { .. } => Self::Missing,
                Effect::CreateDir { .. } => Self::Directory,
                Effect::ReplaceChars { .. } | Effect::Rename { .. } => return None,
                _ => seen,
            };
        }
        Some(seen)
    }
}

/// Follow the target and apply pending size changes, reading contents only when needed.
pub(crate) async fn metadata_through(txn: &TxnHandle, server: &Path) -> io::Result<FileMetadata> {
    let disk = match tokio::fs::metadata(server).await {
        Ok(metadata) if metadata.is_file() => FileMetadata::File(metadata.len()),
        Ok(metadata) if metadata.is_dir() => FileMetadata::Directory,
        Ok(metadata) => FileMetadata::Other(metadata.len()),
        Err(error) if error.kind() == io::ErrorKind::NotFound => FileMetadata::Missing,
        Err(error) => return Err(error),
    };
    if let Some(metadata) = txn.with(|t| t.pending_file_metadata(server, disk)) {
        return Ok(metadata);
    }
    Ok(match read_through(txn, server).await? {
        Seen::Missing => FileMetadata::Missing,
        Seen::Dir => FileMetadata::Directory,
        Seen::File(bytes) => FileMetadata::File(bytes.len() as u64),
    })
}
