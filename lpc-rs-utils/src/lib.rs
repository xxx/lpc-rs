#![forbid(unsafe_code)]

use std::path::Path;

use tokio::fs;

pub mod config;
pub mod debug_log;
pub mod lpc_string;
pub mod string;

/// A convenience helper to handle adding a trailing newline if one isn't there.
/// This is just a thin wrapper around `read_to_string()`
pub async fn read_lpc_file<P>(path: P) -> std::io::Result<String>
where
    P: AsRef<Path>,
{
    fs::read_to_string(path.as_ref()).await.map(|mut x| {
        if !x.ends_with('\n') {
            x.push('\n');
        }
        x
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_read_lpc_file() {
        let with_newline = read_lpc_file("./tests/fixtures/newlines/file_ending_with_newline.h")
            .await
            .unwrap();
        assert!(with_newline.ends_with('\n'));

        let path_without = "./tests/fixtures/newlines/file_not_ending_with_newline.h";
        assert!(
            !fs::read_to_string(path_without)
                .await
                .unwrap()
                .ends_with('\n')
        );
        assert!(read_lpc_file(path_without).await.unwrap().ends_with('\n'));
    }
}
