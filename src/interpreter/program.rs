pub mod code_pool;
mod function;
mod function_scope;
pub(crate) mod linker;

pub use function::Function;
pub use function_scope::FunctionScope;
pub use linker::InheritedProgram;

use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    path::{Path, PathBuf},
    sync::Arc,
};

use derive_builder::Builder;
use indexmap::IndexMap;
use itertools::Itertools;
use lpc_rs_core::{
    INIT_GLOBALS, INIT_PROGRAM, RegisterSize, global_var_flags::GlobalVarFlags, lpc_path::LpcPath,
    lpc_type::LpcType, pragma_flags::PragmaFlags,
};
use lpc_rs_function_support::symbol::Symbol;
use path_dedot::*;
use ustr::{Ustr, existing_ustr};

use crate::interpreter::{lpc_array::LpcArray, stm::SVar};

/// The in-game directory of `path`: its parent with `.`/`..` folded, rooted
/// at `/`.
pub(crate) fn in_game_dir(path: &Path) -> PathBuf {
    match path.parent() {
        None => PathBuf::from("/"),
        Some(dir) => {
            let dedotted = dir.parse_dot_from("/");
            if dir.is_absolute() {
                dedotted.into_owned()
            } else {
                PathBuf::from("/").join(dedotted)
            }
        }
    }
}

/// One program's block of global slots within a program that holds it.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Region {
    /// The program the block belongs to.
    pub filename: Arc<LpcPath>,
    /// The first slot of the block.
    pub base: RegisterSize,
    /// How many slots the block holds.
    pub count: RegisterSize,
    /// The mangled name of the function that initializes the block.
    pub init: Ustr,
}

/// One global declaration, including declarations hidden by inherited name lookup.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GlobalVariable {
    /// The declared name, which need not be unique across ancestors.
    pub name: String,
    /// The source identity of the declaring program.
    pub filename: Arc<LpcPath>,
    /// The declared type, independent of the current value.
    pub type_: LpcType,
    /// Declaration visibility and storage modifiers.
    pub flags: GlobalVarFlags,
    /// The declaration's relocated slot in the containing program.
    pub slot: RegisterSize,
}

/// What a plain call of a mangled name reaches.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Target {
    /// The most-derived definition of the called name.
    pub function: Function,
    /// Whether that is a different function from the one the call was
    /// compiled against.
    pub overridden: bool,
}

/// The [`Program::dispatch`] table of `functions`, which holds inherited
/// functions first, in inherit order, the program's own last.
pub fn dispatch_table(
    functions: &IndexMap<Ustr, Function, ahash::RandomState>,
) -> IndexMap<Ustr, Target, ahash::RandomState> {
    let mut latest: HashMap<&str, &Function> = HashMap::new();
    for function in functions.values().filter(|f| !f.is_closure()) {
        latest.insert(function.prototype.name.as_ref(), function);
    }
    functions
        .iter()
        .map(|(&mangled, function)| {
            let name = function.prototype.name.as_ref();
            let own = function.is_closure()
                || function.prototype.flags.private()
                || name == INIT_GLOBALS
                || name == INIT_PROGRAM;
            let target = if own { function } else { latest[name] };
            (
                mangled,
                Target {
                    function: target.clone(),
                    overridden: !std::ptr::eq(target, function),
                },
            )
        })
        .collect()
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Builder)]
#[builder(default, build_fn(error = "lpc_rs_errors::LpcError"))]
pub struct Program {
    /// The in-game source identity, retaining host origin for unrestricted outside sources.
    #[builder(setter(into))]
    pub filename: Arc<LpcPath>,

    /// Every function by mangled name, inherited ones first. Keyed by the
    /// interned name so a `Call`'s `Ustr` hashes and compares by pointer.
    pub functions: Box<IndexMap<Ustr, Function, ahash::RandomState>>,

    /// Every mangled name mapped to what a plain call of it reaches: the
    /// object's most-derived definition of the same unmangled name, the
    /// last seen in `functions` order. A closure, an initializer or a
    /// `private` function reaches itself.
    pub dispatch: Box<IndexMap<Ustr, Target, ahash::RandomState>>,

    /// Function mapping of unmangled name to the function.
    /// This is needed for `call_other`.
    /// Due to unmangled names not being unique, only the last-defined
    /// function with a given unmangled name is referenced here.
    pub unmangled_functions: Box<IndexMap<String, Function, ahash::RandomState>>,

    /// Function lookup in this program's own source scope.
    pub function_scope: Arc<FunctionScope>,

    /// Source scopes retained for qualified lookup by inherited code.
    pub inherited_function_scopes: Box<HashMap<Arc<LpcPath>, Arc<FunctionScope>>>,

    /// The function that is called when the program is first loaded,
    /// which initializes the global variables. This function is
    /// the combined initializer of all of the inherited programs.
    pub initializer: Option<Function>,

    /// The map of global variables in this program.
    pub global_variables: Box<HashMap<String, Symbol>>,

    /// Every global declaration in slot order, with shared ancestor slots listed once.
    pub global_variable_info: Box<[GlobalVariable]>,

    /// Direct parents in declaration order, including automatically inherited programs.
    pub direct_inherits: Box<[Arc<LpcPath>]>,

    /// How many globals does this program need storage for?
    /// Note that this number includes inherited globals.
    pub num_globals: RegisterSize,

    /// Canonical slots whose cell identities follow the process's ordinary globals.
    pub global_views: Arc<[RegisterSize]>,

    /// Every program whose globals this one holds, in initialization order,
    /// its own block last. A program reached through two parents appears once.
    pub layout: Box<[Region]>,

    /// Which pragmas have been set for this program?
    pub pragmas: PragmaFlags,

    /// The transactional list of clones sharing this compiled program.
    pub clones: SVar<LpcArray>,
}

impl Program {
    pub fn new<T>(filename: T) -> Self
    where
        T: Into<LpcPath>,
    {
        Self {
            filename: Arc::new(filename.into()),
            ..Default::default()
        }
    }

    /// The function with the mangled name `mangled`, inherited ones included.
    #[inline]
    pub fn function(&self, mangled: Ustr) -> Option<&Function> {
        self.functions.get(&mangled)
    }

    /// What a plain call of `mangled` reaches.
    #[inline]
    pub fn target(&self, mangled: Ustr) -> Option<&Target> {
        self.dispatch.get(&mangled)
    }

    /// Look up a function by its unmangled name, then its mangled one,
    /// inherited ones included. The unmangled table comes first because a
    /// `->` arrives here as a string, and the interner's probe on the way
    /// to the mangled table cost call_churn 8%; a name never interned is a
    /// miss, not an interning.
    pub fn lookup_function<T>(&self, name: T) -> Option<&Function>
    where
        T: AsRef<str>,
    {
        let function_name = name.as_ref();
        self.unmangled_functions.get(function_name).or_else(|| {
            existing_ustr(function_name).and_then(|mangled| self.functions.get(&mangled))
        })
    }

    /// Resolve an inherited function in `scope`, before checking caller visibility.
    pub(crate) fn lookup_inherited_function(
        &self,
        scope: &LpcPath,
        namespace: &str,
        name: &str,
    ) -> Option<&Function> {
        let scope = if scope == self.filename.as_ref() {
            &self.function_scope
        } else {
            self.inherited_function_scopes.get(scope)?
        };
        self.function(scope.inherited_function(namespace, name)?)
    }

    /// Whether [`Self::lookup_function`] would find `name`.
    pub fn contains_function<T>(&self, name: T) -> bool
    where
        T: AsRef<str>,
    {
        self.lookup_function(name).is_some()
    }

    /// Get the in-game directory of this program. Used for clone_object, etc.
    pub fn cwd(&self) -> PathBuf {
        in_game_dir(self.filename.as_ref())
    }

    /// The canonical global slot at an index in the execution cell arena.
    pub(crate) fn global_slot(&self, index: usize) -> RegisterSize {
        if index < usize::from(self.num_globals) {
            index as RegisterSize
        } else {
            self.global_views[index - usize::from(self.num_globals)]
        }
    }

    /// Get a listing of this Program's assembly language, suitable for printing
    ///
    /// # Examples
    /// ```
    /// # #[tokio::main(flavor = "current_thread")]
    /// # async fn main() {
    /// use lpc_rs::compiler::{
    ///     Compiler,
    ///     ast::{
    ///         binary_op_node::{BinaryOpNode, BinaryOperation},
    ///         expression_node::ExpressionNode,
    ///         int_node::IntNode,
    ///     },
    ///     codegen::{codegen_walker::CodegenWalker, tree_walker::TreeWalker},
    ///     compilation_context::CompilationContext,
    /// };
    ///
    /// let code = r#"
    ///     void foo() {
    ///         dump("sup?");
    ///     }
    /// "#;
    ///
    /// let compiler = Compiler::default();
    /// let program = compiler
    ///     .compile_string("~/my_file.c", code)
    ///     .await
    ///     .expect("Failed to compile.")
    ///     .program;
    ///
    /// for instruction in program.listing() {
    ///     println!("{}", instruction);
    /// }
    /// # }
    /// ```
    pub fn listing(&self) -> Vec<String> {
        let functions = self
            .functions
            .values()
            .sorted_unstable_by(|a, b| Ord::cmp(&a.name(), &b.name()));

        self.initializer
            .as_ref()
            .map(|init| {
                init.projected(self.num_globals, &self.global_views)
                    .listing()
            })
            .unwrap_or_default()
            .into_iter()
            .chain(functions.into_iter().flat_map(|func| {
                func.projected(self.num_globals, &self.global_views)
                    .listing()
            }))
            .collect()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.filename)
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::{lpc_type::LpcType, mangle::Mangle};
    use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;
    use lpc_rs_function_support::program_function::ProgramFunction;
    use ustr::ustr;

    use super::*;

    #[test]
    fn test_cwd() {
        let mut program = Program {
            filename: Arc::new("foo/bar/baz.c".into()),
            ..Program::default()
        };

        assert_eq!(program.cwd().to_str().unwrap(), "/foo/bar");

        program.filename = Arc::new("marf.c".into());
        assert_eq!(program.cwd().to_str().unwrap(), "/");

        program.filename = Arc::new(LpcPath::in_game(Path::new("").to_path_buf()));
        assert_eq!(program.cwd().to_str().unwrap(), "/");

        program.filename = Arc::new("foo/bar/baz/quux/../../snerd/marf.c".into());
        assert_eq!(program.cwd().to_str().unwrap(), "/foo/bar/snerd");

        program.filename = Arc::new("../foo/bar/marf.c".into());
        assert_eq!(program.cwd().to_str().unwrap(), "/foo/bar");
    }

    #[test]
    fn a_plain_call_reaches_the_last_definition_of_its_name() {
        use lpc_rs_core::function_flags::FunctionFlags;
        use lpc_rs_function_support::function_prototype::{FunctionKind, FunctionPrototypeBuilder};
        use ustr::ustr;

        let function = |name: &str, file: &str, kind, flags| {
            let prototype = FunctionPrototypeBuilder::default()
                .name(name.to_string())
                .filename(Arc::new(file.into()))
                .return_type(LpcType::Void)
                .kind(kind)
                .flags(flags)
                .build()
                .unwrap();
            Arc::new(ProgramFunction::new(prototype, 0))
        };
        let public = FunctionFlags::default();
        let private = FunctionFlags::from(&["private"][..]);
        let parents_g = function("g", "/p.c", FunctionKind::Local, public);
        let parents_h = function("h", "/p.c", FunctionKind::Local, private);
        let parents_init = function(INIT_GLOBALS, "/p.c", FunctionKind::Local, public);
        let closure = function("closure-1", "/p.c", FunctionKind::Closure, public);
        let childs_g = function("g", "/c.c", FunctionKind::Local, public);
        let childs_h = function("h", "/c.c", FunctionKind::Local, public);
        let childs_init = function(INIT_GLOBALS, "/c.c", FunctionKind::Local, public);
        let mut functions: IndexMap<Ustr, Function, ahash::RandomState> = IndexMap::default();
        for f in [
            &parents_g,
            &parents_h,
            &parents_init,
            &closure,
            &childs_g,
            &childs_h,
            &childs_init,
        ] {
            functions.insert(ustr(&f.mangle()), Arc::clone(f).into());
        }

        let dispatch = dispatch_table(&functions);

        let target = |f: &Arc<ProgramFunction>| &dispatch[&ustr(&f.mangle())];
        assert!(Arc::ptr_eq(&target(&parents_g).function.code, &childs_g));
        assert!(target(&parents_g).overridden);
        assert!(Arc::ptr_eq(&target(&childs_g).function.code, &childs_g));
        assert!(!target(&childs_g).overridden);
        assert!(
            Arc::ptr_eq(&target(&parents_h).function.code, &parents_h),
            "private is not overridden"
        );
        assert!(
            Arc::ptr_eq(&target(&parents_init).function.code, &parents_init),
            "an initializer is its own"
        );
        assert!(
            Arc::ptr_eq(&target(&closure).function.code, &closure),
            "a closure is its own"
        );
    }

    fn program_with(name: &'static str) -> Program {
        let prototype = FunctionPrototypeBuilder::default()
            .name(name)
            .filename(Arc::new("/p.c".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let function = Arc::new(ProgramFunction::new(prototype, 0));
        let mut functions: IndexMap<Ustr, Function, ahash::RandomState> = IndexMap::default();
        functions.insert(ustr(&function.mangle()), function.clone().into());
        let mut unmangled: IndexMap<String, Function, ahash::RandomState> = IndexMap::default();
        unmangled.insert(name.to_string(), function.into());
        Program {
            functions: Box::new(functions),
            unmangled_functions: Box::new(unmangled),
            ..Program::default()
        }
    }

    #[test]
    fn function_finds_a_mangled_name_by_its_ustr() {
        let program = program_with("f");
        let mangled = ustr(&program.unmangled_functions["f"].mangle());

        assert!(program.function(mangled).is_some());
        assert!(program.function(ustr("f")).is_none());
    }

    #[test]
    fn lookup_function_takes_a_mangled_name_as_str() {
        let program = program_with("f");
        let mangled = program.unmangled_functions["f"].mangle();

        assert!(program.lookup_function(mangled.as_str()).is_some());
        assert!(program.lookup_function("f").is_some());
    }

    #[test]
    fn a_missed_lookup_interns_nothing() {
        let program = program_with("f");

        assert!(program.lookup_function("never_interned_zzq").is_none());
        assert!(ustr::existing_ustr("never_interned_zzq").is_none());
    }
}
