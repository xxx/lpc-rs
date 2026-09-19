use std::{
    collections::HashMap,
    io,
    path::{Path, PathBuf},
    sync::Arc,
};

use async_trait::async_trait;
use lpc_rs::compiler::source_reader::{DiskSourceReader, SourceKind, SourceReader};
use lsp_types::{Position, Range, Uri};
use url::Url;

#[derive(Debug, Clone)]
pub struct Document {
    pub path: PathBuf,
    pub version: i32,
    pub text: Arc<str>,
}

pub type Documents = HashMap<Uri, Document>;

pub fn file_path(uri: &Uri) -> Option<PathBuf> {
    Url::parse(uri.as_str()).ok()?.to_file_path().ok()
}

pub fn file_uri(path: &Path) -> Option<Uri> {
    Url::from_file_path(path).ok()?.as_str().parse().ok()
}

/// An immutable view of the buffers used for one batch of checks.
#[derive(Debug)]
pub struct Overlay {
    files: HashMap<PathBuf, Arc<str>>,
}

impl Overlay {
    pub fn new(documents: &Documents) -> Self {
        Self {
            files: documents
                .values()
                .map(|doc| (doc.path.clone(), doc.text.clone()))
                .collect(),
        }
    }
}

#[async_trait]
impl SourceReader for Overlay {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        match self.files.get(path) {
            Some(text) => Ok(text.as_bytes().to_vec()),
            None => DiskSourceReader.read(path).await,
        }
    }

    async fn kind(&self, path: &Path) -> io::Result<SourceKind> {
        if self.files.contains_key(path) {
            Ok(SourceKind::File)
        } else {
            DiskSourceReader.kind(path).await
        }
    }
}

pub fn position(text: &str, byte: usize) -> Position {
    let mut byte = byte.min(text.len());
    while !text.is_char_boundary(byte) {
        byte -= 1;
    }
    let prefix = &text[..byte];
    let line = prefix.bytes().filter(|&b| b == b'\n').count();
    let start = prefix.rfind('\n').map_or(0, |index| index + 1);
    let character = prefix[start..]
        .trim_end_matches('\r')
        .encode_utf16()
        .count();
    Position::new(line as u32, character as u32)
}

pub fn byte_offset(text: &str, position: Position) -> Option<usize> {
    let start = if position.line == 0 {
        0
    } else {
        text.match_indices('\n').nth(position.line as usize - 1)?.0 + 1
    };
    let line = text[start..].split('\n').next()?.trim_end_matches('\r');
    let mut units = 0;
    for (offset, ch) in line.char_indices() {
        if units == position.character {
            return Some(start + offset);
        }
        units += ch.len_utf16() as u32;
        if units > position.character {
            return None;
        }
    }
    Some(start + line.len())
}

pub fn range(text: &str, bytes: std::ops::Range<usize>) -> Range {
    Range::new(position(text, bytes.start), position(text, bytes.end))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn positions_handle_utf16_crlf_and_eof() {
        let text = "a💖é\r\nx";
        assert_eq!(position(text, 5), Position::new(0, 3));
        assert_eq!(byte_offset(text, Position::new(0, 3)), Some(5));
        assert_eq!(byte_offset(text, Position::new(0, 2)), None);
        assert_eq!(byte_offset(text, Position::new(0, 100)), Some(7));
        assert_eq!(position(text, text.len()), Position::new(1, 1));
        assert_eq!(byte_offset("x\n", Position::new(1, 0)), Some(2));
    }

    #[test]
    fn file_uris_decode_escaped_paths() {
        let path = std::env::temp_dir().join("mud lib").join("café.c");
        let uri = file_uri(&path).unwrap();
        assert!(uri.as_str().contains("%20"));
        assert_eq!(file_path(&uri), Some(path));
        assert!(file_path(&"untitled:buffer".parse().unwrap()).is_none());
    }
}
