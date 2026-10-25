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
        let file = File::open(server)?;
        let limit = length.unwrap_or(u64::MAX);
        let hint = file
            .metadata()
            .map(|m| m.len().saturating_sub(start as u64).min(limit))
            .unwrap_or(0);
        read_fresh_range(file, start as u64, limit, hint)
    })
    .await
    .map_err(io::Error::other)?
}

/// Read from a fresh handle, seeking only when the requested start is nonzero.
fn read_fresh_range(
    mut reader: impl Read + Seek,
    start: u64,
    limit: u64,
    hint: u64,
) -> io::Result<Option<Vec<u8>>> {
    if start > 0 && reader.seek(io::SeekFrom::Start(start)).is_err() {
        return Ok(None);
    }
    let mut bytes = Vec::new();
    bytes
        .try_reserve(usize::try_from(hint).unwrap_or(usize::MAX))
        .map_err(io::Error::other)?;
    reader.take(limit).read_to_end(&mut bytes)?;
    Ok(Some(bytes))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::lib_holding;

    #[test]
    fn fresh_readers_can_start_at_zero_without_seeking() {
        struct Unseekable(io::Cursor<Vec<u8>>);

        impl Read for Unseekable {
            fn read(&mut self, bytes: &mut [u8]) -> io::Result<usize> {
                self.0.read(bytes)
            }
        }

        impl Seek for Unseekable {
            fn seek(&mut self, _position: io::SeekFrom) -> io::Result<u64> {
                Err(io::Error::new(
                    io::ErrorKind::Unsupported,
                    "seeking is unavailable",
                ))
            }
        }

        let reader = || Unseekable(io::Cursor::new(b"abcdef".to_vec()));
        assert_eq!(
            read_fresh_range(reader(), 0, 3, 6).unwrap(),
            Some(b"abc".to_vec())
        );
        assert_eq!(read_fresh_range(reader(), 2, 3, 6).unwrap(), None);
    }

    #[tokio::test]
    async fn unchanged_files_support_bounded_reads_from_the_start_and_middle() {
        let root = lib_holding("bounded-file-ranges", &[("data", "abcdefgh")]);
        let path = root.join("data");
        let txn = TxnHandle::empty();
        for (start, length, expected) in [
            (0, Some(3), b"abc".as_slice()),
            (3, Some(2), b"de".as_slice()),
            (8, Some(3), b"".as_slice()),
            (2, None, b"cdefgh".as_slice()),
        ] {
            assert_eq!(
                read_disk_range(&txn, &path, start, length).await.unwrap(),
                Some(expected.to_vec())
            );
        }
    }
}
