use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;

use crate::{
    ast::expression_node::ExpressionNode,
    semantic::{function_prototype::FunctionPrototype, scope_tree::ScopeTree},
};

use crate::errors::LpcError;
use std::sync::Arc;

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
    pub include_dirs: Arc<Vec<PathBuf>>,

    /// Our collection of scopes
    pub scopes: ScopeTree,

    /// The map of function names, to their respective prototypes.
    /// Used for checking forward references.
    pub function_prototypes: HashMap<String, FunctionPrototype>,

    /// Storage for default function params, for the functions that have them
    pub function_params: HashMap<String, Vec<Option<ExpressionNode>>>,

    /// Any errors that have been collected
    pub errors: Vec<LpcError>,
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
            root_dir: PathBuf::from(root_dir.as_ref())
                .absolutize()
                .unwrap()
                .to_path_buf(),
            include_dirs: Arc::new(include_dirs.iter().map(|i| PathBuf::from(*i)).collect()),
            ..Self::default()
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            filename: String::from(""),
            root_dir: PathBuf::from("."),
            include_dirs: Arc::new(Vec::new()),
            errors: Vec::new(),
            scopes: ScopeTree::default(),
            function_params: HashMap::new(),
            function_prototypes: HashMap::new(),
        }
    }
}
