use std::collections::HashMap;

use crate::{
    ast::expression_node::ExpressionNode,
    semantic::{function_prototype::FunctionPrototype, scope_tree::ScopeTree},
};

use crate::{errors::LpcError, interpreter::pragma_flags::PragmaFlags, util::config::Config};
use std::{path::Path, rc::Rc};

/// A big, fat state object to store data created at various stages of compilation.
/// A single one of these will be used for loading/compiling a single file (files `#include`d in
/// that file will share this state object when they are compiled, as well.)
#[derive(Debug)]
pub struct Context {
    /// The name of the main file being compiled.
    pub filename: String,

    pub config: Rc<Config>,

    /// Our collection of scopes
    pub scopes: ScopeTree,

    /// The map of function names, to their respective prototypes.
    /// Used for checking forward references.
    pub function_prototypes: HashMap<String, FunctionPrototype>,

    /// Storage for default function params, for the functions that have them
    pub function_params: HashMap<String, Vec<Option<ExpressionNode>>>,

    /// Any errors that have been collected
    pub errors: Vec<LpcError>,

    /// The pragmas that have been set
    pub pragmas: PragmaFlags,
}

impl Context {
    /// Create a new `Context`
    ///
    /// # Arguments
    ///
    /// `filename` - The path to the file (from `root_dir`) this context will be collected for.
    /// `config` - The [`Config`] from `config.toml` or the command line
    ///
    /// # Examples
    /// ```
    /// use std::rc::Rc;
    /// use lpc_rs::context::Context;
    /// use lpc_rs::util::config::Config;
    ///
    /// let context = Context::new("./test.c", Rc::new(Config::default()));
    /// ```
    pub fn new<T>(filename: T, config: Rc<Config>) -> Self
    where
        T: AsRef<Path>,
    {
        Self {
            filename: filename.as_ref().to_string_lossy().into_owned(),
            config,
            ..Self::default()
        }
    }

    #[inline]
    pub fn lib_dir(&self) -> &str {
        self.config.lib_dir()
    }

    #[inline]
    pub fn system_include_dirs(&self) -> &Vec<String> {
        self.config.system_include_dirs()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            filename: String::from(""),
            config: Rc::new(Config::default()),
            errors: Vec::new(),
            scopes: ScopeTree::default(),
            function_params: HashMap::new(),
            function_prototypes: HashMap::new(),
            pragmas: PragmaFlags::new(),
        }
    }
}
