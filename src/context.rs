use crate::errors::lazy_files::LazyFiles;
use path_absolutize::Absolutize;
use std::path::{PathBuf, Path};

/// A big, fat state object to store data created at various stages of compilation.
/// A single one of these will be used for loading/compiling a single file (files `#include`d in
/// that file will share this state object when they are compiled, as well.)
#[derive(Debug)]
pub struct Context {
    /// The name of the main file being compiled.
    pub filename: String,

    /// The true on-disk directory where the in-game root directory starts.
    /// (sometimes known as `LIBDIR`, etc.)
    pub root_dir: PathBuf,

    /// In-game directories that will be searched for system `#include`s.
    /// Searches will be done in the order given by this vector.
    pub include_dirs: Vec<PathBuf>,

    /// A filestore, used heavily to get code spans for error messages.
    pub files: LazyFiles<String, String>,
}

impl Context {
    /// Create a new `Context`
    ///
    /// # Arguments
    ///
    /// `filename` - The path to the file (from `root_dir`) this context will be collected for.
    /// `root_dir` - The path to the root of the in-game filesystem
    /// `include_dirs` - A vector of *in-game* paths to be used for searching for system includes.
    ///     Searches will be done in the order given by this vector.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::context::Context;
    ///
    /// let context = Context::new("./test.c", "/home/mud/lib", vec!["/include", "/sys"]);
    /// ```
    pub fn new<T, U>(filename: T, root_dir: U, include_dirs: Vec<&str>) -> Self
    where
        T: AsRef<str>,
        U: AsRef<Path>,
    {
        Self {
            filename: String::from(filename.as_ref()),
            root_dir: PathBuf::from(root_dir.as_ref()).absolutize().unwrap().to_path_buf(),
            include_dirs: include_dirs.iter().map(|i| PathBuf::from(*i)).collect(),
            ..Self::default()
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            filename: String::from(""),
            root_dir: PathBuf::from("."),
            include_dirs: vec![],
            files: LazyFiles::new(),
        }
    }
}
