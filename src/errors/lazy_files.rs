use crate::parser::span::Span;
use cached::{proc_macro::cached, SizedCache};
use codespan_reporting::files::{Error, Files, SimpleFile};
use fs_err as fs;
use lazy_static::lazy_static;
use parking_lot::RwLock;
use std::{
    ffi::{OsStr, OsString},
    marker::PhantomData,
    ops::Range,
    path::Path,
};

#[derive(Debug)]
enum LazyFile<Name, Source> {
    // A file path, that's lazily-loaded if and when necessary.
    Lazy(Name),

    // A filepath and its source code. This is mostly used in testing.
    Eager(Name, Source),
}

lazy_static! {
    /// A global file cache for use in error reporting.
    pub static ref FILE_CACHE: RwLock<LazyFiles<String, String>> = RwLock::new(LazyFiles::new());
}

/// A wrapper type for the global [`FILE_CACHE`](struct@crate::errors::lazy_files::FILE_CACHE)
pub struct FileCache;

impl FileCache {
    /// A convenience helper to add a file to the mutexed global file cache.
    /// This function will block if it needs to wait for the mutex.
    ///
    /// # Arguments
    /// `path` - The path of the file to add. It will be used as the key in the file cache.
    pub fn insert<T>(path: T) -> FileId
    where
        T: std::fmt::Display,
    {
        let mut cache = FILE_CACHE.write();
        cache.add(path.to_string())
    }
}

/// For readability
pub type FileId = usize;

/// A memoizing lazy-loaded Files store for [`codespan-reporting`](codespan_reporting)
/// # Examples
///
/// ```
/// use lpc_rs::errors::lazy_files::LazyFiles;
///
/// let mut files: LazyFiles<&str, String> = LazyFiles::new();
/// let path = "tests/fixtures/code/include/simple.h";
/// let id = files.add(&path);
///
/// println!("contents: {}", files.get(id).unwrap().source());
/// println!("contents: {}", files.get_by_path(&path).unwrap().source());
///
/// // Also handles eagerly-adding code to the cache
/// let id2 = files.add_eager("my-in-memory-file.c", String::from("int j = 123;"));
/// assert_eq!(files.get(id2).unwrap().source(), "int j = 123;");
/// ```
#[derive(Debug)]
pub struct LazyFiles<Name, Source>
where
    Name: AsRef<Path> + Clone + std::fmt::Display,
{
    paths: Vec<LazyFile<Name, Source>>,
    source: PhantomData<Source>,
}

impl<'a, Name, Source> LazyFiles<Name, Source>
where
    Name: AsRef<Path> + Clone + std::fmt::Display + std::cmp::PartialEq,
    Source: std::fmt::Display + AsRef<str>,
{
    /// Create a new [`LazyFiles`] instance
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new file to the cache
    ///
    /// # Arguments
    /// `path` - The absolute on-server path, including filename, to the file being added.
    pub fn add(&mut self, path: Name) -> FileId {
        if let Some(id) = self.id_for(&path) {
            return id;
        }

        let id = self.paths.len();
        self.paths.push(LazyFile::Lazy(path));
        id
    }

    /// Add a new file plus its source code to the cache
    ///
    /// # Arguments
    /// `path` - The absolute full path, including filename, to the file being added.
    /// `code` - The source code for the file in question
    pub fn add_eager(&mut self, path: Name, code: Source) -> FileId {
        if let Some(id) = self.id_for(&path) {
            return id;
        }

        let id = self.paths.len();
        self.paths.push(LazyFile::Eager(path, code));
        id
    }

    /// Get a file by its id
    ///
    /// # Arguments
    /// `id` - The ID of the file to get (IDs are returned by `add()`).
    pub fn get(&self, id: FileId) -> Result<SimpleFile<String, String>, Error> {
        match self.paths.get(id) {
            Some(lf) => match lf {
                LazyFile::Lazy(path) => self.get_by_path(path),
                LazyFile::Eager(path, source) => {
                    Ok(SimpleFile::new(path.to_string(), source.to_string()))
                }
            },
            None => Err(Error::FileMissing),
        }
    }

    /// Get a file by its path
    ///
    /// # Arguments
    /// `path` - The full path, including filename, to the file.
    pub fn get_by_path<T>(&self, path: T) -> Result<SimpleFile<String, String>, Error>
    where
        T: AsRef<Path>,
    {
        cached_file(path.as_ref().as_os_str())
    }

    /// Get the `FileId` for the passed path
    ///
    /// # Arguments
    /// `path` - The path of the file stored in the cache
    pub fn id_for(&self, path: &Name) -> Option<FileId> {
        self.paths.iter().position(|i| match i {
            LazyFile::Lazy(p) => path == p,
            LazyFile::Eager(p, _) => path == p,
        })
    }

    /// Get a `Span` for a specific `file_id` and line
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
            file_id,
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
/// `path` - An [`OsStr`] reference representing a full path.
#[cached(
    result = true,
    type = "SizedCache<OsString, SimpleFile<String, String>>",
    create = "{ SizedCache::with_size(5) }",
    convert = r#"{ OsString::from(path) }"#
)]
fn cached_file(path: &OsStr) -> Result<SimpleFile<String, String>, Error> {
    let source = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => return Err(Error::from(e)),
    };

    Ok(SimpleFile::new(
        String::from(path.to_string_lossy()),
        source,
    ))
}

impl<'input, Name, Source> Files<'input> for LazyFiles<Name, Source>
where
    Name: 'input + std::fmt::Display + Clone + AsRef<Path> + std::cmp::PartialEq,
    Source: std::fmt::Display + AsRef<str>,
{
    type FileId = FileId;
    type Name = Name;
    type Source = String;

    fn name(&self, id: Self::FileId) -> Result<Self::Name, Error> {
        match self.paths.get(id) {
            Some(s) => {
                let p = match s {
                    LazyFile::Lazy(p) => p,
                    LazyFile::Eager(p, _) => p,
                };

                Ok(p.clone())
            }
            None => Err(Error::FileMissing),
        }
    }

    fn source(&self, id: Self::FileId) -> Result<Self::Source, Error> {
        Ok(String::from(self.get(id)?.source()))
    }

    fn line_index(&self, id: Self::FileId, byte_index: usize) -> Result<usize, Error> {
        self.get(id)?.line_index((), byte_index)
    }

    fn line_range(&self, id: Self::FileId, line_index: usize) -> Result<Range<usize>, Error> {
        self.get(id)?.line_range((), line_index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn does_not_duplicate() {
        let mut files: LazyFiles<&str, String> = LazyFiles::new();
        let file = "/foo/bar.c";

        let id = files.add(file);
        assert_eq!(id, files.add(file));
    }

    #[test]
    fn handles_inline_source() {
        let mut files: LazyFiles<&str, &str> = LazyFiles::new();
        let file = "/foo/bar.c";
        let prog = "int j = 123;";

        let id = files.add_eager(file, prog);
        assert_eq!(files.get(id).unwrap().source(), prog);
    }
}
