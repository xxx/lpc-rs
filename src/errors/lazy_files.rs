use std::{fs, fmt};
use std::ops::Range;
use cached::proc_macro::cached;
use codespan_reporting::files::{Files, Error as CodespanError};
use std::path::Path;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;

#[derive(Debug)]
struct LazyFilesError<'a>(&'a str);

impl<'a> Display for LazyFilesError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> Error for LazyFilesError<'a> {}

// struct LazyFile<Name, Source> {
//     paths: Name,
//     source: Source,
//     line_indexes: Vec<usize>
// }

/// A memoizing lazy-loaded Files store for `codespan-reporting`
pub struct LazyFiles<Name, Source> {
    paths: Vec<Name>,
    source: PhantomData<Source>
}

impl<'a, Name, Source> LazyFiles<Name, Source> {
    pub fn new() -> Self {
        Self {
            paths: Vec::new(),
            source: PhantomData
        }
    }

    /// Add a new file to the cache
    /// 
    /// # Arguments
    /// `path` - The full path (including filename) to the file being added.
    ///     No canonicalization is done, so it's highly recommended to use
    ///     absolute paths.
    pub fn add(&mut self, path: Name) -> usize {
        let id = self.paths.len();
        self.paths.push(path);
        id
    }
}

/// Memoized function that simply stores the content of a file at the given path.
#[cached(size = 3, result = true)]
fn cached_source(path: String) -> Result<String, CodespanError> {
    match fs::read_to_string(path) {
        Ok(content) => Ok(content),
        Err(e) => Err(CodespanError::from(e))
    }
}

impl<'input, Name, Source> Files<'input> for LazyFiles<Name, Source>
where
    Name: 'input + std::fmt::Display + Clone + AsRef<Path>
{
    type FileId = usize;
    type Name = Name;
    type Source = String;

    fn name(&self, id: Self::FileId) -> Result<Self::Name, CodespanError> {
        match self.paths.get(id) {
            Some(s) => Ok(s.clone()),
            _ => Err(CodespanError::FileMissing)
        }
    }

    fn source(&self, id: Self::FileId) -> Result<Self::Source, CodespanError> {
        let name = self.name(id)?;

        match cached_source(name.to_string()) {
            Ok(s) => Ok(s),
            Err(err) => Err(err)
        }
    }

    fn line_index(&self, id: Self::FileId, byte_index: usize) -> Result<usize, CodespanError> {
        unimplemented!()
    }

    fn line_range(&self, id: Self::FileId, line_index: usize) -> Result<Range<usize>, CodespanError> {
        unimplemented!()
    }
}