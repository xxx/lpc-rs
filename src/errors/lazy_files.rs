use crate::parser::span::Span;
use cached::{proc_macro::cached, SizedCache};
use codespan_reporting::files::{Error as CodespanError, Files, SimpleFile};
use std::{
    ffi::{OsStr, OsString},
    fs,
    marker::PhantomData,
    ops::Range,
    path::Path,
};
use lazy_static::lazy_static;
use parking_lot::RwLock;

lazy_static! {
    /// A global file cache for use in error reporting.
    pub static ref FILE_CACHE: RwLock<LazyFiles<String, String>> = RwLock::new(LazyFiles::new());
}

/// A convenience helper to add a file to the mutexed global file cache.
/// This function will block if it needs to wait for the mutex.
///
/// # Arguments
/// `path` - The path of the file to add. It will be used as the key in the file cache.
pub fn add_file_to_cache<T>(path: T) -> FileId
where
    T: std::fmt::Display
{
    let mut cache = FILE_CACHE.write();
    cache.add(path.to_string())
}

/// For readability
pub type FileId = usize;

/// A memoizing lazy-loaded Files store for `codespan-reporting`
///
/// # Examples
///
/// ```
/// use lpc_rs::errors::lazy_files::LazyFiles;
///
/// let mut files: LazyFiles<&str, String> = LazyFiles::new();
/// let path = "tests/fixtures/include/simple.h";
/// let id = files.add(&path);
///
/// println!("contents: {}", files.get(id).unwrap().source());
/// println!("contents: {}", files.get_by_path(&path).unwrap().source());
/// ```
#[derive(Debug)]
pub struct LazyFiles<Name, Source>
where
    Name: AsRef<Path> + Clone + std::fmt::Display,
{
    paths: Vec<Name>,
    source: PhantomData<Source>,
}

impl<'a, Name, Source> LazyFiles<Name, Source>
where
    Name: AsRef<Path> + Clone + std::fmt::Display + std::cmp::PartialEq,
{
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new file to the cache
    ///
    /// # Arguments
    /// `path` - The full path, including filename, to the file being added.
    ///     No canonicalization is done, so it's highly recommended to use
    ///     absolute paths.
    pub fn add(&mut self, path: Name) -> usize {
        let id = self.paths.len();
        self.paths.push(path);
        id
    }

    /// Get a file by its id
    ///
    /// # Arguments
    /// `id` - The ID of the file to get (IDs are returned by `add()`).
    pub fn get(&self, id: FileId) -> Result<SimpleFile<String, String>, CodespanError> {
        let path = self.name(id)?;

        self.get_by_path(path)
    }

    /// Get a file by its path
    ///
    /// # Arguments
    /// `path` - The full path, including filename, to the file.
    pub fn get_by_path<T>(&self, path: T) -> Result<SimpleFile<String, String>, CodespanError>
    where
        T: AsRef<Path>,
    {
        Ok(cached_file(path.as_ref().as_os_str())?)
    }

    /// Get the FileId for the passed path
    ///
    /// # Arguments
    /// `path` - The path of the file stored in the cache
    pub fn get_id(&self, path: Name) -> Option<FileId>
    {
        self.paths.iter().position(|i| *i == path)
    }

    /// Get a `Span` for a specific file_id and line
    ///
    /// # Arguments
    /// `file_id` - the file ID you're looking for
    /// `line_num` - the line to get, `0`-indexed.
    pub fn file_line_span(&self, file_id: FileId, line_num: usize) -> Span {
        let range = if let Ok(r) = self.line_range(file_id, line_num) {
            r
        } else {
            0..1
        };

        Span {
            l: range.start,
            r: range.end - 1,
        }
    }
}

impl<Name, Source> Default for LazyFiles<Name, Source>
where
    Name: AsRef<Path> + Clone + std::fmt::Display,
{
    fn default() -> Self {
        Self {
            paths: Vec::new(),
            source: PhantomData,
        }
    }
}

/// Memoized function that simply stores the content of a file at the given path.
/// Concrete types are used here because of memoization
///
/// # Arguments
/// `path` - An OsString representing a full path.
#[cached(
    result = true,
    type = "SizedCache<OsString, SimpleFile<String, String>>",
    create = "{ SizedCache::with_size(5) }",
    convert = r#"{ OsString::from(path) }"#
)]
fn cached_file(path: &OsStr) -> Result<SimpleFile<String, String>, CodespanError> {
    let source = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => return Err(CodespanError::from(e)),
    };

    Ok(SimpleFile::new(
        String::from(path.to_string_lossy()),
        source,
    ))
}

impl<'input, Name, Source> Files<'input> for LazyFiles<Name, Source>
where
    Name: 'input + std::fmt::Display + Clone + AsRef<Path> + std::cmp::PartialEq,
{
    type FileId = FileId;
    type Name = Name;
    type Source = String;

    fn name(&self, id: Self::FileId) -> Result<Self::Name, CodespanError> {
        match self.paths.get(id) {
            Some(s) => Ok(s.clone()),
            _ => Err(CodespanError::FileMissing),
        }
    }

    fn source(&self, id: Self::FileId) -> Result<Self::Source, CodespanError> {
        Ok(String::from(self.get(id)?.source()))
    }

    fn line_index(&self, id: Self::FileId, byte_index: usize) -> Result<usize, CodespanError> {
        self.get(id)?.line_index((), byte_index)
    }

    fn line_range(
        &self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, CodespanError> {
        self.get(id)?.line_range((), line_index)
    }
}
//
// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn test_cache_works() {
//         let mut files: LazyFiles<&str, String> = LazyFiles::new();
//         let file = "/etc/issue";
//
//         let id = files.add(file);
//
//         println!("{}", files.source(id).unwrap());
//         println!("{}", files.source(id).unwrap());
//     }
// }
