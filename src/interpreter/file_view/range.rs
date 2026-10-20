//! Bounded reads of unchanged regular files.

use std::{
    fs::File,
    io::{self, Read, Seek},
    path::Path,
};

use crate::interpreter::stm::TxnHandle;

/// Read a disk range, or return `None` when the full content view is needed.
pub(crate) async fn read_disk_range(
    txn: &TxnHandle,
    server: &Path,
    start: i64,
    length: Option<u64>,
) -> io::Result<Option<Vec<u8>>> {
    // Negative and empty ranges use the actual content length, not a metadata hint.
    if start < 0 || length == Some(0) || txn.with(|t| t.has_pending_file_ops(server)) {
        return Ok(None);
    }
    let server = server.to_path_buf();
    tokio::task::spawn_blocking(move || {
        if !std::fs::metadata(&server)?.is_file() {
            return Ok(None);
        }
        let mut file = File::open(server)?;
        if start > 0 && file.seek(io::SeekFrom::Start(start as u64)).is_err() {
            return Ok(None);
        }
        let limit = length.unwrap_or(u64::MAX);
        let hint = file
            .metadata()
            .map(|m| m.len().saturating_sub(start as u64).min(limit))
            .unwrap_or(0);
        let mut bytes = Vec::new();
        bytes
            .try_reserve(usize::try_from(hint).unwrap_or(usize::MAX))
            .map_err(io::Error::other)?;
        file.take(limit).read_to_end(&mut bytes)?;
        Ok(Some(bytes))
    })
    .await
    .map_err(io::Error::other)?
}
