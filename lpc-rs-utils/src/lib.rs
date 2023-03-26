use std::{fs, path::Path};

use lpc_rs_errors::lazy_files::FILE_CACHE;

pub mod config;
pub mod string;

/// A convenience helper to handle adding a trailing newline if one isn't there.
/// This is just a thin wrapper around `read_to_string()`
pub fn read_lpc_file<P>(path: P) -> std::io::Result<String>
where
    P: AsRef<Path>,
{
    fs::read_to_string(path.as_ref())
        .or_else(|e| {
            // Fall back to checking the cache, for the case of code added eagerly.
            // This shouldn't be reached in normal use, but is highly useful for testing and debugging.
            let files = FILE_CACHE.read();
            // TODO: rectify the need for calling to_string on the path
            path.as_ref()
                .to_str()
                .and_then(|p| files.id_for(&p.to_string()))
                .and_then(|id| files.get(id).ok().map(|file| file.source().clone()))
                .ok_or(e)
        })
        .map(|mut x| {
            if !x.ends_with('\n') {
                x.push('\n');
            }
            x
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_lpc_file() {
        let with_newline =
            read_lpc_file("./tests/fixtures/newlines/file_ending_with_newline.h").unwrap();
        assert!(with_newline.ends_with('\n'));

        let path_without = "./tests/fixtures/newlines/file_not_ending_with_newline.h";
        assert!(!fs::read_to_string(path_without).unwrap().ends_with('\n'));
        assert!(read_lpc_file(path_without).unwrap().ends_with('\n'));

        let eager_path = "./tests/fixtures/newlines/does_not_exist.h";
        let eager_contents = "eager beaver";
        {
            let mut cache = FILE_CACHE.write();
            cache.add_eager(eager_path, eager_contents);
        }
        assert_eq!(
            read_lpc_file(eager_path).unwrap(),
            format!("{eager_contents}\n")
        );
    }
}
