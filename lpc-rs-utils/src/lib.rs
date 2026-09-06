#![forbid(unsafe_code)]

use std::path::Path;

use tokio::fs;

pub mod config;
pub mod debug_log;
pub mod lpc_string;
pub mod string;

/// A source file's text, read from disk with a trailing newline appended
/// if one wasn't there.
pub struct LpcSource {
    /// The file's text.
    pub text: String,
    /// Whether the file was not valid UTF-8 and so was read as Latin-1.
    pub latin1: bool,
}

/// Read a source file's bytes as UTF-8, falling back to Latin-1
/// (`b as char`, a one-to-one mapping of a byte to its code point, not a
/// lossy decode) so a stray non-UTF-8 byte doesn't fail the read.
pub async fn read_lpc_file<P>(path: P) -> std::io::Result<LpcSource>
where
    P: AsRef<Path>,
{
    let bytes = fs::read(path.as_ref()).await?;
    let (mut text, latin1) = match String::from_utf8(bytes) {
        Ok(text) => (text, false),
        Err(e) => (
            e.into_bytes().into_iter().map(|b| b as char).collect(),
            true,
        ),
    };
    if !text.ends_with('\n') {
        text.push('\n');
    }
    Ok(LpcSource { text, latin1 })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_read_lpc_file() {
        let with_newline = read_lpc_file("./tests/fixtures/newlines/file_ending_with_newline.h")
            .await
            .unwrap();
        assert!(with_newline.text.ends_with('\n'));
        assert!(!with_newline.latin1);

        let path_without = "./tests/fixtures/newlines/file_not_ending_with_newline.h";
        assert!(
            !fs::read_to_string(path_without)
                .await
                .unwrap()
                .ends_with('\n')
        );
        assert!(
            read_lpc_file(path_without)
                .await
                .unwrap()
                .text
                .ends_with('\n')
        );
    }

    #[tokio::test]
    async fn a_non_utf8_file_reads_as_latin1() {
        let dir = std::env::temp_dir().join(format!(
            "lpc-rs-utils-latin1-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("caf.c");
        std::fs::write(&path, b"int x = 1; // caf\xe9\n").unwrap();

        let source = read_lpc_file(&path).await.unwrap();

        assert!(source.latin1);
        assert!(source.text.contains('é'), "{:?}", source.text);
    }

    #[tokio::test]
    async fn a_utf8_file_is_not_flagged_latin1() {
        let source = read_lpc_file("./tests/fixtures/newlines/file_ending_with_newline.h")
            .await
            .unwrap();
        assert!(!source.latin1);
    }
}
