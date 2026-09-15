//! Source reads and path probes shared by the compiler and its include walk.

use std::{fmt::Debug, io, path::Path};

use async_trait::async_trait;

/// The filesystem distinction needed when resolving compiler inputs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    File,
    Directory,
    Other,
}

/// Supplies source bytes independently of compile-time authorization.
#[async_trait]
pub trait SourceReader: Debug + Send + Sync {
    /// Read a file's bytes, before decoding or newline normalization.
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>>;

    /// Probe a path without reading its contents; missing paths return `NotFound`.
    async fn kind(&self, path: &Path) -> io::Result<SourceKind>;
}

/// Host-facing compiles read directly from disk.
#[derive(Debug)]
pub struct DiskSourceReader;

#[async_trait]
impl SourceReader for DiskSourceReader {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        tokio::fs::read(path).await
    }

    async fn kind(&self, path: &Path) -> io::Result<SourceKind> {
        let metadata = tokio::fs::metadata(path).await?;
        Ok(if metadata.is_file() {
            SourceKind::File
        } else if metadata.is_dir() {
            SourceKind::Directory
        } else {
            SourceKind::Other
        })
    }
}
