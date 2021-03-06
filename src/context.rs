use crate::errors::lazy_files::LazyFiles;
use std::path::PathBuf;
use path_absolutize::Absolutize;

/// A big, fat state object to store data created at various stages of compilation.
/// The single one of these will be used for loading/compiling a single file (files `#include`d in
/// that file will share this state object when they are compiled, as well.)
#[derive(Debug)]
pub struct Context {
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
    /// `root_dir` - The path to the root of the in-game filesystem
    /// `include_dirs` - A vector of *in-game* paths to be used for searching for system includes.
    ///     Searches will be done in the order given by this vector.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::context::Context;
    ///
    /// let context = Context::new("/home/mud/lib", vec!["/include", "/sys"]);
    /// ```
    pub fn new(root_dir: &str, include_dirs: Vec<&str>) -> Self {
        Self {
            root_dir: PathBuf::from(root_dir).absolutize().unwrap().to_path_buf(),
            include_dirs: include_dirs.iter().map(|i| PathBuf::from(*i)).collect(),
            ..Self::default()
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            root_dir: PathBuf::from("."),
            include_dirs: vec![],
            files: LazyFiles::new()
        }
    }
}