use std::collections::HashMap;

use crate::{
    ast::expression_node::ExpressionNode,
    interpreter::efun::EFUN_PROTOTYPES,
    semantic::{function_prototype::FunctionPrototype, scope_tree::ScopeTree},
};

use crate::{
    core::lpc_type::LpcType,
    errors::LpcError,
    interpreter::{pragma_flags::PragmaFlags, program::Program},
    semantic::symbol::Symbol,
    util::{config::Config, path_maker::LpcPath},
};
use delegate::delegate;
use std::rc::Rc;

/// A big, fat state object to store data created at various stages of compilation.
/// A single one of these will be used for loading/compiling a single file (files `#include`d in
/// that file will share this state object when they are compiled, as well.)
/// Inherited files will have their own.
#[derive(Debug)]
pub struct CompilationContext {
    /// The name of the main file being compiled.
    pub filename: LpcPath,

    /// The configuration being used for this compilation.
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
    /// The ordering of this field can be assumed to be in the order of declaration
    pub inherits: Vec<Program>,

    /// The index of name -> inherited objects, for inherits with names
    pub inherit_names: HashMap<String, usize>,

    /// How deep into an inheritance chain is this context?
    pub inherit_depth: usize,

    /// How many global variables have been declared in inherited-from parents?
    /// This is how we determine how much space the final [`Process`] needs to
    /// allocate for global variables.
    pub num_globals: usize,

    /// How many [`Register`]s were required for initializing global variables,
    /// in inherited-from parents?
    /// This is how we determine how much space the final [`Process`] needs to
    /// allocate for the global `init-program` call, when an object is cloned.
    pub num_init_registers: usize,
}

impl CompilationContext {
    delegate! {
        to self.config {
            /// config's lib_dir (a.k.a. LIB_DIR)
            pub fn lib_dir(&self) -> &str;

            /// config's system include directories
            pub fn system_include_dirs(&self) -> &Vec<String>;
        }
    }

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

    /// Set the `inherit_depth` of the context
    pub fn with_inherit_depth(mut self, depth: usize) -> Self {
        self.inherit_depth = depth;
        self
    }

    /// Set the `global_variable_count` of the context
    pub fn with_global_variable_count(mut self, count: usize) -> Self {
        self.num_globals = count;
        self
    }

    /// Set the `global_register_count` of the context
    pub fn with_global_register_count(mut self, count: usize) -> Self {
        self.num_init_registers = count;
        self
    }

    /// Look-up a function by name, then check inherited parents if not found
    pub fn lookup_function<T>(&self, name: T) -> Option<&FunctionPrototype>
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();

        self.function_prototypes.get(r).or_else(|| {
            self.inherits
                .iter()
                .rev()
                .find_map(|p| p.lookup_function(r).map(|f| &f.prototype))
        })
    }

    /// Look-up a function locally, and fall back to checking the efuns if a
    /// function with the passed name isn't found either locally or in
    /// inherited-from parents.
    pub fn lookup_function_complete<T>(&self, name: T) -> Option<&FunctionPrototype>
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();
        self.lookup_function(r).or_else(|| EFUN_PROTOTYPES.get(r))
    }

    /// Do I, or one of my parents, contain a function with this name?
    pub fn contains_function(&self, name: &str) -> bool {
        self.function_prototypes.contains_key(name)
            || self
                .inherits
                .iter()
                .rev()
                .any(|p| p.contains_function(name))
    }

    /// Convenience function to check if a function is available anywhere that
    /// I am allowed access.
    pub fn contains_function_complete(&self, name: &str) -> bool {
        self.contains_function(name) || EFUN_PROTOTYPES.contains_key(name)
    }

    /// Look-up a variable by name, then check inherited parents if not found
    pub fn lookup_var<T>(&self, name: T) -> Option<&Symbol>
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();

        self.scopes.lookup(r).or_else(|| {
            self.inherits
                .iter()
                .rev()
                .find_map(|p| p.global_variables.get(r))
        })
    }

    /// Get a mutable reference to a variable by name, checking inherited parents if not found
    pub fn lookup_var_mut<T>(&mut self, name: T) -> Option<&mut Symbol>
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();

        self.scopes.lookup_mut(r).or_else(|| {
            self.inherits
                .iter_mut()
                .rev()
                .find_map(|p| p.global_variables.get_mut(r))
        })
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
            num_globals: 0,
            num_init_registers: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        core::function_arity::FunctionArity,
        semantic::{function_flags::FunctionFlags, program_function::ProgramFunction},
    };

    fn make_function_prototype(name: &'static str) -> FunctionPrototype {
        FunctionPrototype::new(
            name,
            LpcType::Int(false),
            FunctionArity::new(0),
            FunctionFlags::default(),
            None,
            Vec::new(),
            Vec::new(),
        )
    }

    fn make_program_function(name: &'static str) -> ProgramFunction {
        let prototype = make_function_prototype(name);

        ProgramFunction::new(prototype, 0)
    }

    #[test]
    fn test_lookup_function() {
        let mut context = CompilationContext::default();
        let mut inherited = Program::default();

        let proto = make_function_prototype("foo");
        context
            .function_prototypes
            .insert("foo".into(), proto.clone());

        let efun_override = make_function_prototype("this_object");
        context
            .function_prototypes
            .insert("this_object".into(), efun_override.clone());

        let overridden = make_program_function("foo");
        inherited.functions.insert("foo".into(), overridden.into());

        let inherited_proto = make_program_function("hello_friends");
        inherited
            .functions
            .insert("hello_friends".into(), inherited_proto.clone().into());

        context.inherits.push(inherited);

        assert_eq!(
            // gets from the inherited parent
            context.lookup_function("hello_friends"),
            Some(&inherited_proto.prototype)
        );

        assert_eq!(
            // gets the local version
            context.lookup_function("foo"),
            Some(&proto)
        );

        assert_eq!(
            // gets the more local overridden version
            context.lookup_function("this_object"),
            Some(&efun_override)
        );

        assert_eq!(
            // not defined
            context.lookup_function("bar"),
            None
        );

        assert_eq!(
            // efun
            context.lookup_function("dump"),
            None
        );
    }

    #[test]
    fn test_lookup_function_complete() {
        let mut context = CompilationContext::default();
        let mut inherited = Program::default();

        let proto = make_function_prototype("foo");
        context
            .function_prototypes
            .insert("foo".into(), proto.clone());

        let efun_override = make_function_prototype("this_object");
        context
            .function_prototypes
            .insert("this_object".into(), efun_override.clone());

        let overridden = make_program_function("foo");
        inherited.functions.insert("foo".into(), overridden.into());

        let inherited_proto = make_program_function("hello_friends");
        inherited
            .functions
            .insert("hello_friends".into(), inherited_proto.clone().into());

        context.inherits.push(inherited);

        assert_eq!(
            // gets from the inherited parent
            context.lookup_function_complete("hello_friends"),
            Some(&inherited_proto.prototype)
        );

        assert_eq!(
            // gets the local version
            context.lookup_function_complete("foo"),
            Some(&proto)
        );

        assert_eq!(
            // gets the more local overridden version
            context.lookup_function_complete("this_object"),
            Some(&efun_override)
        );

        assert_eq!(
            // not defined
            context.lookup_function_complete("bar"),
            None
        );

        assert_eq!(
            // efun
            context.lookup_function_complete("dump"),
            Some(EFUN_PROTOTYPES.get("dump").unwrap())
        );
    }

    #[test]
    fn test_contains_function() {
        let mut context = CompilationContext::default();
        let mut inherited = Program::default();

        let proto = make_function_prototype("foo");
        context
            .function_prototypes
            .insert("foo".into(), proto.clone());

        let efun_override = make_function_prototype("this_object");
        context
            .function_prototypes
            .insert("this_object".into(), efun_override.clone());

        let overridden = make_program_function("foo");
        inherited.functions.insert("foo".into(), overridden.into());

        let inherited_proto = make_program_function("hello_friends");
        inherited
            .functions
            .insert("hello_friends".into(), inherited_proto.clone().into());

        context.inherits.push(inherited);

        assert_eq!(
            // gets from the inherited parent
            context.contains_function("hello_friends"),
            true
        );

        assert_eq!(
            // gets the local version
            context.contains_function("foo"),
            true
        );

        assert_eq!(
            // gets the more local overridden version
            context.contains_function("this_object"),
            true
        );

        assert_eq!(
            // not defined
            context.contains_function("bar"),
            false
        );

        assert_eq!(
            // efun
            context.contains_function("dump"),
            false
        );
    }

    #[test]
    fn test_contains_function_complete() {
        let mut context = CompilationContext::default();
        let mut inherited = Program::default();

        let proto = make_function_prototype("foo");
        context
            .function_prototypes
            .insert("foo".into(), proto.clone());

        let efun_override = make_function_prototype("this_object");
        context
            .function_prototypes
            .insert("this_object".into(), efun_override.clone());

        let overridden = make_program_function("foo");
        inherited.functions.insert("foo".into(), overridden.into());

        let inherited_proto = make_program_function("hello_friends");
        inherited
            .functions
            .insert("hello_friends".into(), inherited_proto.clone().into());

        context.inherits.push(inherited);

        assert_eq!(
            // gets from the inherited parent
            context.contains_function_complete("hello_friends"),
            true
        );

        assert_eq!(
            // gets the local version
            context.contains_function_complete("foo"),
            true
        );

        assert_eq!(
            // gets the more local overridden version
            context.contains_function_complete("this_object"),
            true
        );

        assert_eq!(
            // not defined
            context.contains_function_complete("bar"),
            false
        );

        assert_eq!(
            // efun
            context.contains_function_complete("dump"),
            true
        );
    }

    #[test]
    fn test_lookup_var_and_lookup_var_mut() {
        let mut context = CompilationContext::default();
        context.scopes.push_new();
        let mut earlier_inherit = Program::default();
        let mut inherited = Program::default();

        // this one should not be found
        let early_inherited_global = Symbol::new("my_inherited_global", LpcType::Float(false));
        earlier_inherit.global_variables.insert(
            "my_inherited_global".into(),
            early_inherited_global.clone().into(),
        );

        let mut inherited_global = Symbol::new("my_inherited_global", LpcType::Int(false));
        inherited.global_variables.insert(
            "my_inherited_global".into(),
            inherited_global.clone().into(),
        );

        let overridden_global = Symbol::new("overridden", LpcType::Int(false));
        inherited
            .global_variables
            .insert("overridden".into(), overridden_global.clone().into());

        context.inherits.push(earlier_inherit);
        context.inherits.push(inherited);

        let mut global = Symbol::new("my_global", LpcType::Int(false));
        context.scopes.current_mut().unwrap().insert(global.clone());

        let mut overriding_local = Symbol::new("overridden", LpcType::String(false));
        context
            .scopes
            .current_mut()
            .unwrap()
            .insert(overriding_local.clone());

        assert_eq!(
            // gets from the inherited parent
            context.lookup_var("my_inherited_global"),
            Some(&inherited_global)
        );

        assert_eq!(
            // gets the more local overridden version
            context.lookup_var("overridden"),
            Some(&overriding_local)
        );

        assert_eq!(context.lookup_var("my_global"), Some(&global));

        assert_eq!(
            // gets from the inherited parent
            context.lookup_var_mut("my_inherited_global"),
            Some(&mut inherited_global)
        );

        assert_eq!(
            // gets the more local overridden version
            context.lookup_var_mut("overridden"),
            Some(&mut overriding_local)
        );

        assert_eq!(context.lookup_var_mut("my_global"), Some(&mut global));
    }
}
