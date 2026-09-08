//! A file as the calling task will leave it: the disk, with the task's own
//! pending effects on that path applied in order, so a read sees what an
//! earlier write in the same task will commit.

use std::{
    io::{Error, ErrorKind},
    path::Path,
};

use crate::interpreter::{efun::efun_context::EfunContext, stm::PendingFileOp};

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

/// `server` as this efun's task will leave it: the disk with the task's
/// pending changes to the path applied. A rename's source is read as it is
/// on disk.
pub(crate) async fn read_through<const N: usize>(
    context: &EfunContext<'_, N>,
    server: &Path,
) -> std::io::Result<Seen> {
    let mut seen = on_disk(server).await?;
    for op in context.pending_file_ops(server) {
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
            PendingFileOp::CopyOf(from) => on_disk(&from).await?,
            PendingFileOp::WriteBytes { start, contents } => match seen {
                Seen::File(mut bytes) => {
                    let start = usize::try_from(start).unwrap_or(usize::MAX);
                    let end = start.saturating_add(contents.len());
                    if bytes.len() < end {
                        bytes.resize(end, 0);
                    }
                    bytes[start..end].copy_from_slice(contents.as_bytes());
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
