use cached::{proc_macro::cached, SizedCache};
use codespan_reporting::files::{Error as CodespanError, Files, SimpleFile};
use std::{
    ffi::{OsStr, OsString},
    fs,
    marker::PhantomData,
    ops::Range,
    path::Path,
};

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
    Name: AsRef<Path> + Clone + std::fmt::Display,
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
    pub fn get(&self, id: usize) -> Result<SimpleFile<String, String>, CodespanError> {
        let path = self.name(id)?;

        self.get_by_path(path)
    }

    /// Get a file by its path
    ///
    /// # Arguments
    /// `path` - The full path, including filename, so the file.
    pub fn get_by_path<T>(&self, path: T) -> Result<SimpleFile<String, String>, CodespanError>
    where
        T: AsRef<Path>,
    {
        Ok(cached_file(path.as_ref().as_os_str())?)
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
    Name: 'input + std::fmt::Display + Clone + AsRef<Path>,
{
    type FileId = usize;
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