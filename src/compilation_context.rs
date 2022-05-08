use std::collections::HashMap;

use crate::{
    ast::expression_node::ExpressionNode,
    interpreter::efun::EFUN_PROTOTYPES,
    semantic::{function_prototype::FunctionPrototype, scope_tree::ScopeTree},
};

use crate::{
    errors::LpcError,
    interpreter::{pragma_flags::PragmaFlags, program::Program},
    semantic::lpc_type::LpcType,
    util::{config::Config, path_maker::LpcPath},
};
use std::rc::Rc;

/// A big, fat state object to store data created at various stages of compilation.
/// A single one of these will be used for loading/compiling a single file (files `#include`d in
/// that file will share this state object when they are compiled, as well.)
/// Inherited files will have their own.
#[derive(Debug)]
pub struct CompilationContext {
    /// The name of the main file being compiled.
    pub filename: LpcPath,

    pub config: Rc<Config>,

    /// Our collection of scopes
    pub scopes: ScopeTree,

    /// The map of function names, to their respective prototypes.
    /// Used for checking forward references.
    pub function_prototypes: HashMap<String, FunctionPrototype>,

    /// Storage for default function params, for the functions that have them
    pub default_function_params: HashMap<String, Vec<Option<ExpressionNode>>>,

    /// Any errors that have been collected
    pub errors: Vec<LpcError>,

    /// The pragmas that have been set
    pub pragmas: PragmaFlags,

    /// All of my Inherited parent objects
    /// The ordering of this field can be assumed to be in file order
    pub inherits: Vec<Program>,

    /// The index of name -> inherited objects, for inherits with names
    pub inherit_names: HashMap<String, usize>,

    /// How deep into an inheritance chain is this context?
    pub inherit_depth: usize,
}

impl CompilationContext {
    /// Create a new `Context`
    ///
    /// # Arguments
    ///
    /// `filename` - The path to the file (relative to config's `root_dir`) this context will be collected for.
    /// `config` - The [`Config`] from `config.toml` or the command line
    ///
    /// # Examples
    /// ```
    /// use std::rc::Rc;
    /// use lpc_rs::compilation_context::CompilationContext;
    /// use lpc_rs::util::config::Config;
    ///
    /// let context = CompilationContext::new("./test.c", Rc::new(Config::default()));
    /// ```
    pub fn new<T>(filename: T, config: Rc<Config>) -> Self
    where
        T: Into<LpcPath>,
    {
        Self {
            filename: filename.into(),
            config,
            ..Self::default()
        }
    }

    /// Set the inherit_depth of the context
    pub fn with_inherit_depth(mut self, depth: usize) -> Self {
        self.inherit_depth = depth;
        self
    }

    #[inline]
    pub fn lib_dir(&self) -> &str {
        self.config.lib_dir()
    }

    #[inline]
    pub fn system_include_dirs(&self) -> &Vec<String> {
        self.config.system_include_dirs()
    }

    /// Look-up a function by name
    pub fn lookup_function<T>(&self, name: T) -> Option<&FunctionPrototype>
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();
        self.function_prototypes.get(r)
    }

    /// Look-up a function locally, and fall back to checking the efuns if a local function with
    /// the passed name isn't found locally.
    pub fn lookup_function_complete<T>(&self, name: T) -> Option<&FunctionPrototype>
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();
        if let Some(prototype) = self.function_prototypes.get(r) {
            Some(prototype)
        } else {
            EFUN_PROTOTYPES.get(r)
        }
    }

    /// A transformation helper to get a map of function names to their return types.
    pub fn function_return_types(&self) -> HashMap<&str, LpcType> {
        self.function_prototypes
            .keys()
            .map(|k| k.as_str())
            .zip(self.function_prototypes.values().map(|v| v.return_type))
            .collect::<HashMap<_, _>>()
    }
}

impl Default for CompilationContext {
    fn default() -> Self {
        Self {
            filename: LpcPath::default(),
            config: Rc::new(Config::default()),
            errors: Vec::new(),
            scopes: ScopeTree::default(),
            default_function_params: HashMap::new(),
            function_prototypes: HashMap::new(),
            pragmas: PragmaFlags::new(),
            inherits: Vec::new(),
            inherit_names: HashMap::new(),
            inherit_depth: 0,
        }
    }
}
