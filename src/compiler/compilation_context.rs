use std::{collections::HashMap, rc::Rc, sync::Arc};

use derive_builder::Builder;
use educe::Educe;
use indexmap::{IndexMap, IndexSet};
use lpc_rs_core::{
    call_namespace::CallNamespace, lpc_path::LpcPath, pragma_flags::PragmaFlags, EFUN,
};
use lpc_rs_errors::LpcError;
use lpc_rs_function_support::{
    function_like::FunctionLike, function_prototype::FunctionPrototype,
    program_function::ProgramFunction, symbol::Symbol,
};
use lpc_rs_utils::config::Config;
use qcell::{QCell, QCellOwner};
use string_interner::StringInterner;

use crate::{
    compiler::{ast::expression_node::ExpressionNode, semantic::scope_tree::ScopeTree},
    interpreter::{efun::EFUN_PROTOTYPES, process::Process, program::Program},
    util::qcell_process_option_debug,
};

/// A big, fat state object to store data created at various stages of
/// compilation. A single one of these will be used for loading/compiling a
/// single file (files `#include`d in that file will share this state object
/// when they are compiled, as well.) Inherited files will have their own.
#[derive(Educe, Builder)]
#[educe(Debug)]
#[builder(default, build_fn(error = "lpc_rs_errors::LpcError"))]
pub struct CompilationContext {
    /// The name of the main file being compiled.
    #[builder(setter(into))]
    pub filename: Arc<LpcPath>,

    /// The configuration being used for this compilation.
    #[builder(setter(into))]
    pub config: Rc<Config>,

    /// Our collection of scopes
    pub scopes: ScopeTree,

    /// The map of function names, to their respective prototypes.
    /// Used for checking forward references, and other things.
    pub function_prototypes: HashMap<String, FunctionPrototype>,

    /// Storage for default function params, for the functions that have them
    pub default_function_params: HashMap<String, Vec<Option<ExpressionNode>>>,

    /// Any warnings & errors that have been collected
    pub errors: Vec<LpcError>,

    /// The pragmas that have been set
    pub pragmas: PragmaFlags,

    /// Strings table
    pub strings: StringInterner,

    /// All of the inherited functions, keyed by their mangled name.
    pub inherited_functions: IndexMap<String, Rc<ProgramFunction>>,

    /// All of my Inherited parent objects
    /// The ordering of this field can be assumed to be in the order of
    /// declaration
    pub inherits: Vec<Program>,

    /// The index of name -> inherited objects, for inherits with names
    pub inherit_names: HashMap<String, usize>,

    /// How deep into an inheritance chain is this context?
    pub inherit_depth: usize,

    /// How many global variables have been declared in inherited-from parents?
    /// This is how we determine how much space the final [`Process`] needs to
    /// allocate for global variables.
    pub num_globals: usize,

    /// How many variables need to be upvalued?
    pub num_upvalues: usize,

    /// How many [`Register`](lpc_rs_core::register::Register)s were required
    /// for initializing global variables, in inherited-from parents?
    /// This is how we determine how much space the final [`Process`] needs to
    /// allocate for the global `init-program` call, when an object is cloned.
    pub num_init_registers: usize,

    /// Pointer to the simul efuns
    #[educe(Debug(method = "qcell_process_option_debug"))]
    pub simul_efuns: Option<Rc<QCell<Process>>>,

    /// The count of closures that have been defined, so we can give them unique
    /// names.
    pub closure_count: usize,
}

impl CompilationContext {
    /// config's lib_dir (a.k.a. LIB_DIR)
    pub fn lib_dir(&self) -> &str {
        &self.config.lib_dir
    }

    /// config's system include directories
    pub fn system_include_dirs(&self) -> &Vec<String> {
        &self.config.system_include_dirs
    }

    /// Look-up a function by name, then check inherited parents if not found
    pub fn lookup_function<T>(
        &self,
        name: T,
        namespace: &CallNamespace,
    ) -> Option<&FunctionPrototype>
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();

        let find_in_inherit = || {
            // look up in reverse, so later declarations override earlier ones
            self.inherits
                .iter()
                .rev()
                .find_map(|inherit| inherit.lookup_function(r).map(|f| &f.prototype))
        };

        match namespace {
            CallNamespace::Local => self.function_prototypes.get(r).or_else(find_in_inherit),
            CallNamespace::Parent => find_in_inherit(),
            CallNamespace::Named(ns) => match ns.as_str() {
                EFUN => EFUN_PROTOTYPES.get(r),
                ns => self.inherit_names.get(ns).and_then(|i| {
                    self.inherits
                        .get(*i)
                        .and_then(|p| p.lookup_function(name).map(|f| &f.prototype))
                }),
            },
        }
    }

    /// Look-up a function locally, and fall back to checking the efuns if a
    /// function with the passed name isn't found either locally or in
    /// inherited-from parents.
    pub fn lookup_function_complete<T>(
        &self,
        name: T,
        namespace: &CallNamespace,
        cell_key: &QCellOwner,
    ) -> Option<FunctionLike>
    where
        T: AsRef<str>,
    {
        if !self.valid_namespace(namespace) {
            return None;
        }

        let nm = name.as_ref();
        // This ugly nest looks locally, then up to inherits, then simul efuns, then
        // efuns
        self.lookup_function(nm, namespace)
            .map(FunctionLike::from)
            .or_else(|| {
                if namespace == &CallNamespace::Local {
                    self.simul_efuns
                        .as_ref()
                        .and_then(|rc| {
                            rc.ro(cell_key)
                                .as_ref()
                                .lookup_function(nm)
                                .map(|f| FunctionLike::from(f.clone()))
                        })
                        .or_else(|| EFUN_PROTOTYPES.get(nm).map(FunctionLike::from))
                } else {
                    None
                }
            })
    }

    fn valid_namespace(&self, ns: &CallNamespace) -> bool {
        match ns {
            CallNamespace::Local | CallNamespace::Parent => true,
            CallNamespace::Named(ns) => match ns.as_str() {
                EFUN => true,
                ns => self.inherit_names.contains_key(ns),
            },
        }
    }

    /// Do I, or one of my parents, contain a function with this name?
    pub fn contains_function(&self, name: &str, namespace: &CallNamespace) -> bool {
        let find_in_inherit = || {
            self.inherits
                .iter()
                .rev()
                .any(|p| p.contains_function(name))
        };

        match namespace {
            CallNamespace::Local => {
                self.function_prototypes.contains_key(name) || find_in_inherit()
            }
            CallNamespace::Parent => find_in_inherit(),
            CallNamespace::Named(ns) => match ns.as_str() {
                EFUN => EFUN_PROTOTYPES.contains_key(name),
                ns => self
                    .inherit_names
                    .get(ns)
                    .and_then(|i| self.inherits.get(*i).map(|p| p.contains_function(name)))
                    .unwrap_or(false),
            },
        }
    }

    /// Convenience function to check if a function is available anywhere that
    /// I am allowed access.
    pub fn contains_function_complete(
        &self,
        name: &str,
        namespace: &CallNamespace,
        cell_key: &QCellOwner,
    ) -> bool {
        if !self.valid_namespace(namespace) {
            return false;
        }

        if self.contains_function(name, namespace) {
            return true;
        }

        if namespace == &CallNamespace::Local {
            return self
                .simul_efuns
                .as_ref()
                .map(|rc| rc.ro(cell_key).as_ref().contains_function(name))
                .unwrap_or(false)
                || EFUN_PROTOTYPES.contains_key(name);
            // return EFUN_PROTOTYPES.contains_key(name);
        }

        false
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

    /// Get a mutable reference to a variable by name, checking inherited
    /// parents if not found
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
}

impl Default for CompilationContext {
    fn default() -> Self {
        Self {
            filename: LpcPath::default().into(),
            config: Rc::new(Config::default()),
            errors: vec![],
            scopes: ScopeTree::default(),
            default_function_params: HashMap::new(),
            function_prototypes: HashMap::new(),
            pragmas: PragmaFlags::new(),
            strings: StringInterner::with_capacity(32),
            inherits: vec![],
            inherit_names: HashMap::new(),
            inherited_functions: IndexMap::new(),
            inherit_depth: 0,
            num_globals: 0,
            num_upvalues: 0,
            num_init_registers: 0,
            simul_efuns: None,
            closure_count: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_type::LpcType;
    use lpc_rs_function_support::{
        function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunction,
    };

    use super::*;

    fn make_function_prototype(name: &'static str) -> FunctionPrototype {
        FunctionPrototypeBuilder::default()
            .name(name)
            .filename(Arc::new(name.into()))
            .return_type(LpcType::Int(false))
            .build()
            .unwrap()
    }

    fn make_program_function(name: &'static str) -> ProgramFunction {
        let prototype = make_function_prototype(name);

        ProgramFunction::new(prototype, 0)
    }

    #[test]
    #[allow(clippy::bool_assert_comparison)]
    fn test_lookups() {
        let cell_key = QCellOwner::new();
        let mut context = CompilationContext::default();
        let mut inherited = Program::default();
        let mut named_inherit = Program::default();
        let mut simul_efuns = Program::default();

        let proto = make_function_prototype("foo");
        context
            .function_prototypes
            .insert("foo".into(), proto.clone());

        let efun_override = make_function_prototype("this_object");
        context
            .function_prototypes
            .insert("this_object".into(), efun_override.clone());

        let overridden = make_program_function("foo");
        inherited
            .functions
            .insert("foo".into(), overridden.clone().into());

        let named_overridden = make_program_function("foo");
        named_inherit
            .functions
            .insert("foo".into(), named_overridden.clone().into());

        let inherited_proto = make_program_function("hello_friends");
        inherited
            .functions
            .insert("hello_friends".into(), inherited_proto.clone().into());

        let simul_efun = make_program_function("simul_efun");
        simul_efuns
            .functions
            .insert("simul_efun".into(), simul_efun.clone().into());

        context.inherits.push(named_inherit);
        context.inherits.push(inherited);

        context.inherit_names.insert("my_named_inherit".into(), 0);

        let proc = cell_key.cell(Process::new(simul_efuns));
        context.simul_efuns = Some(proc.into());
        // lookup_function

        assert_eq!(
            // gets from the inherited parent
            context.lookup_function("hello_friends", &CallNamespace::Local),
            Some(&inherited_proto.prototype)
        );

        assert_eq!(
            // gets the local version
            context.lookup_function("foo", &CallNamespace::Local),
            Some(&proto)
        );

        assert_eq!(
            // gets the parent version
            context.lookup_function("foo", &CallNamespace::Parent),
            Some(&overridden.prototype)
        );

        assert_eq!(
            // gets the more local overridden version
            context.lookup_function("this_object", &CallNamespace::Local),
            Some(&efun_override)
        );

        assert_eq!(
            // efun namespace
            context.lookup_function("this_object", &CallNamespace::Named("efun".into())),
            EFUN_PROTOTYPES.get("this_object")
        );

        assert_eq!(
            // specifically-named namespace
            context.lookup_function("foo", &CallNamespace::Named("my_named_inherit".into())),
            Some(&named_overridden.prototype)
        );

        assert_eq!(
            // cannot get to efuns through non `efun` namespaces
            context.lookup_function("dump", &CallNamespace::Named("my_named_inherit".into())),
            None
        );

        assert_eq!(
            // unknown namespace
            context.lookup_function("this_object", &CallNamespace::Named("blargh".into())),
            None
        );

        assert_eq!(
            // not defined
            context.lookup_function("bar", &CallNamespace::Local),
            None
        );

        assert_eq!(
            // efun
            context.lookup_function("dump", &CallNamespace::Local),
            None
        );

        assert_eq!(
            context.lookup_function("simul_efun", &CallNamespace::Local),
            None
        );

        assert_eq!(
            // not through parent
            context.lookup_function("dump", &CallNamespace::Parent),
            None
        );

        assert_eq!(
            context.lookup_function("simul_efun", &CallNamespace::Parent),
            None
        );

        // lookup_function_complete

        assert_eq!(
            // gets from the inherited parent
            context
                .lookup_function_complete("hello_friends", &CallNamespace::Local, &cell_key)
                .unwrap()
                .prototype(),
            &inherited_proto.prototype
        );

        assert_eq!(
            // gets the local version
            context
                .lookup_function_complete("foo", &CallNamespace::Local, &cell_key)
                .unwrap()
                .prototype(),
            &proto
        );

        assert_eq!(
            // gets the parent version
            context
                .lookup_function_complete("foo", &CallNamespace::Parent, &cell_key)
                .unwrap()
                .prototype(),
            &overridden.prototype
        );

        assert_eq!(
            // gets the more local overridden version
            context
                .lookup_function_complete("this_object", &CallNamespace::Local, &cell_key)
                .unwrap()
                .prototype(),
            &efun_override
        );

        assert_eq!(
            // efun namespace
            context
                .lookup_function_complete(
                    "this_object",
                    &CallNamespace::Named("efun".into()),
                    &cell_key
                )
                .unwrap()
                .prototype(),
            EFUN_PROTOTYPES.get("this_object").unwrap()
        );

        assert_eq!(
            // specifically-named namespace
            context
                .lookup_function_complete(
                    "foo",
                    &CallNamespace::Named("my_named_inherit".into()),
                    &cell_key
                )
                .unwrap()
                .prototype(),
            &named_overridden.prototype
        );

        assert_eq!(
            // cannot get to efuns through non `efun` namespaces
            context.lookup_function_complete(
                "dump",
                &CallNamespace::Named("my_named_inherit".into()),
                &cell_key
            ),
            None
        );

        assert_eq!(
            // unknown namespace
            context.lookup_function_complete(
                "this_object",
                &CallNamespace::Named("blargh".into()),
                &cell_key
            ),
            None
        );

        assert_eq!(
            // not defined
            context.lookup_function_complete("bar", &CallNamespace::Local, &cell_key),
            None
        );

        assert_eq!(
            // efun
            context
                .lookup_function_complete("dump", &CallNamespace::Local, &cell_key)
                .unwrap()
                .prototype(),
            EFUN_PROTOTYPES.get("dump").unwrap()
        );

        assert_eq!(
            // efun
            context
                .lookup_function_complete("simul_efun", &CallNamespace::Local, &cell_key)
                .unwrap()
                .prototype(),
            &simul_efun.prototype
        );

        assert_eq!(
            // not through parent
            context.lookup_function_complete("dump", &CallNamespace::Parent, &cell_key),
            None
        );

        assert_eq!(
            // not through parent
            context.lookup_function_complete("simul_efun", &CallNamespace::Parent, &cell_key),
            None
        );

        // contains_function

        assert_eq!(
            // gets from the inherited parent
            context.contains_function("hello_friends", &CallNamespace::Local),
            true
        );

        assert_eq!(
            // gets the local version
            context.contains_function("foo", &CallNamespace::Local),
            true
        );

        assert_eq!(
            // gets the parent version
            context.contains_function("foo", &CallNamespace::Parent),
            true
        );

        assert_eq!(
            // gets the more local overridden version
            context.contains_function("this_object", &CallNamespace::Local),
            true
        );

        assert_eq!(
            // efun namespace
            context.contains_function("this_object", &CallNamespace::Named("efun".into())),
            true
        );

        assert_eq!(
            // specifically-named namespace
            context.contains_function("foo", &CallNamespace::Named("my_named_inherit".into())),
            true
        );

        assert_eq!(
            // cannot get to efuns through non `efun` namespaces
            context.contains_function("dump", &CallNamespace::Named("my_named_inherit".into())),
            false
        );

        assert_eq!(
            // unknown namespace
            context.contains_function("this_object", &CallNamespace::Named("blargh".into())),
            false
        );

        assert_eq!(
            // not defined
            context.contains_function("bar", &CallNamespace::Local),
            false
        );

        assert_eq!(
            // efun
            context.contains_function("dump", &CallNamespace::Local),
            false
        );

        assert_eq!(
            // efun
            context.contains_function("simul_efun", &CallNamespace::Local),
            false
        );

        assert_eq!(
            // not through parent
            context.contains_function("dump", &CallNamespace::Parent),
            false
        );

        assert_eq!(
            context.contains_function("simul_efun", &CallNamespace::Parent),
            false
        );

        // contains_function_complete

        assert_eq!(
            // gets from the inherited parent
            context.contains_function_complete("hello_friends", &CallNamespace::Local, &cell_key),
            true
        );

        assert_eq!(
            // gets the local version
            context.contains_function_complete("foo", &CallNamespace::Local, &cell_key),
            true
        );

        assert_eq!(
            // gets the parent version
            context.contains_function_complete("foo", &CallNamespace::Parent, &cell_key),
            true
        );

        assert_eq!(
            // gets the more local overridden version
            context.contains_function_complete("this_object", &CallNamespace::Local, &cell_key),
            true
        );

        assert_eq!(
            // efun namespace
            context.contains_function_complete(
                "this_object",
                &CallNamespace::Named("efun".into()),
                &cell_key
            ),
            true
        );

        assert_eq!(
            // specifically-named namespace
            context.contains_function_complete(
                "foo",
                &CallNamespace::Named("my_named_inherit".into()),
                &cell_key,
            ),
            true
        );

        assert_eq!(
            // cannot get to efuns through non `efun` namespaces
            context.contains_function_complete(
                "dump",
                &CallNamespace::Named("my_named_inherit".into()),
                &cell_key,
            ),
            false
        );

        assert_eq!(
            // unknown namespace
            context.contains_function_complete(
                "this_object",
                &CallNamespace::Named("blargh".into()),
                &cell_key
            ),
            false
        );

        assert_eq!(
            // not defined
            context.contains_function_complete("bar", &CallNamespace::Local, &cell_key),
            false
        );

        assert_eq!(
            // efun
            context.contains_function_complete("dump", &CallNamespace::Local, &cell_key),
            true
        );

        assert_eq!(
            // efun
            context.contains_function_complete("simul_efun", &CallNamespace::Local, &cell_key),
            true
        );

        assert_eq!(
            // not through parent
            context.contains_function_complete("dump", &CallNamespace::Parent, &cell_key),
            false
        );

        assert_eq!(
            context.contains_function_complete("simul_efun", &CallNamespace::Parent, &cell_key),
            false
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
        earlier_inherit
            .global_variables
            .insert("my_inherited_global".into(), early_inherited_global);

        let mut inherited_global = Symbol::new("my_inherited_global", LpcType::Int(false));
        inherited
            .global_variables
            .insert("my_inherited_global".into(), inherited_global.clone());

        let overridden_global = Symbol::new("overridden", LpcType::Int(false));
        inherited
            .global_variables
            .insert("overridden".into(), overridden_global);

        context.inherits.push(earlier_inherit);
        context.inherits.push(inherited);

        let global = Symbol::new("my_global", LpcType::Function(true));
        context.scopes.current_mut().unwrap().insert(global);

        let overriding_local = Symbol::new("overridden", LpcType::String(false));
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
            context.lookup_var("overridden").unwrap().type_,
            overriding_local.type_
        );

        assert_eq!(
            context.lookup_var("my_global").unwrap().type_,
            LpcType::Function(true)
        );

        assert_eq!(
            // gets from the inherited parent
            context.lookup_var_mut("my_inherited_global"),
            Some(&mut inherited_global)
        );

        assert_eq!(
            // gets the more local overridden version
            context.lookup_var_mut("overridden").unwrap().type_,
            overriding_local.type_
        );

        assert_eq!(
            context.lookup_var_mut("my_global").unwrap().type_,
            LpcType::Function(true)
        );
    }
}
