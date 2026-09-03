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
    RegisterSize,
    lpc_path::LpcPath,
    pragma_flags::PragmaFlags,
    register::{Register, RegisterVariant},
};
use lpc_rs_function_support::{program_function::ProgramFunction, symbol::Symbol};
use path_dedot::*;
use ustr::{Ustr, existing_ustr};

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

#[derive(Debug, Default, PartialEq, Eq, Clone, Builder)]
#[builder(default, build_fn(error = "lpc_rs_errors::LpcError"))]
pub struct Program {
    /// The path to the file that this program was compiled from.
    /// This is intended to be the fully-expanded, in-game path.
    #[builder(setter(into))]
    pub filename: Arc<LpcPath>,

    /// Every function by mangled name, inherited ones first. Keyed by the
    /// interned name so a `Call`'s `Ustr` hashes and compares by pointer.
    pub functions: Box<IndexMap<Ustr, Arc<ProgramFunction>, ahash::RandomState>>,

    /// Function mapping of unmangled name to the function.
    /// This is needed for `call_other`.
    /// Due to unmangled names not being unique, only the last-defined
    /// function with a given unmangled name is referenced here.
    pub unmangled_functions: Box<IndexMap<String, Arc<ProgramFunction>, ahash::RandomState>>,

    /// The function that is called when the program is first loaded,
    /// which initializes the global variables. This function is
    /// the combined initializer of all of the inherited programs.
    pub initializer: Option<Arc<ProgramFunction>>,

    /// The map of global variables in this program.
    pub global_variables: Box<HashMap<String, Symbol>>,

    /// How many globals does this program need storage for?
    /// Note that this number includes inherited globals.
    pub num_globals: RegisterSize,

    /// Every program whose globals this one holds, in initialization order,
    /// its own block last. A program reached through two parents appears once.
    pub layout: Box<[Region]>,

    /// Which pragmas have been set for this program?
    pub pragmas: PragmaFlags,
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
    pub fn function(&self, mangled: Ustr) -> Option<&Arc<ProgramFunction>> {
        self.functions.get(&mangled)
    }

    /// Look up a function by its unmangled name, then its mangled one,
    /// inherited ones included. The unmangled table comes first because a
    /// `->` arrives here as a string, and the interner's probe on the way
    /// to the mangled table cost call_churn 8%; a name never interned is a
    /// miss, not an interning.
    pub fn lookup_function<T>(&self, name: T) -> Option<&Arc<ProgramFunction>>
    where
        T: AsRef<str>,
    {
        let function_name = name.as_ref();
        self.unmangled_functions.get(function_name).or_else(|| {
            existing_ustr(function_name).and_then(|mangled| self.functions.get(&mangled))
        })
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

    /// Move each block of `layout` to the slot in `targets` at the same
    /// index — blocks two parents share land on one target.
    pub fn relocate_globals(&mut self, targets: &[RegisterSize]) {
        debug_assert_eq!(targets.len(), self.layout.len());
        let layout = std::mem::take(&mut self.layout);
        if layout.iter().zip(targets).all(|(r, &t)| r.base == t) {
            self.layout = layout;
            return;
        }

        let relocate = |register: RegisterVariant| match register {
            RegisterVariant::Global(reg) => {
                let index = reg.index();
                let (position, region) = layout
                    .iter()
                    .enumerate()
                    .find(|(_, r)| (r.base..r.base + r.count).contains(&index))
                    .expect("every global slot is in a layout block");
                RegisterVariant::Global(Register(targets[position] + index - region.base))
            }
            other => other,
        };
        for func in self.functions.values_mut() {
            let mut moved = ProgramFunction::clone(func);
            moved.rename_registers(relocate);
            *func = Arc::new(moved);
        }
        for symbol in self.global_variables.values_mut() {
            symbol.location = symbol.location.map(relocate);
        }
        self.layout = layout
            .iter()
            .zip(targets)
            .map(|(region, &base)| Region {
                base,
                ..region.clone()
            })
            .collect();
    }

    /// Get a listing of this Program's assembly language, suitable for printing
    ///
    /// # Examples
    /// ```
    /// # tokio_test::block_on(async {
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
    /// # });
    /// ```
    pub fn listing(&self) -> Vec<String> {
        let functions = self
            .functions
            .values()
            .sorted_unstable_by(|a, b| Ord::cmp(&a.name(), &b.name()));

        self.initializer
            .as_ref()
            .map(|init| init.listing())
            .unwrap_or_default()
            .into_iter()
            .chain(functions.into_iter().flat_map(|func| func.listing()))
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
    use ustr::ustr;

    use super::*;

    #[test]
    fn test_cwd() {
        let mut program = Program {
            filename: Arc::new("foo/bar/baz.c".into()),
            ..Program::default()
        };

        assert_eq!(program.cwd().to_str().unwrap(), format!("/foo/bar"));

        program.filename = Arc::new("marf.c".into());
        assert_eq!(program.cwd().to_str().unwrap(), "/");

        program.filename = Arc::new(LpcPath::Server(Path::new("").to_path_buf()));
        assert_eq!(program.cwd().to_str().unwrap(), "/");

        program.filename = Arc::new("foo/bar/baz/quux/../../snerd/marf.c".into());
        assert_eq!(program.cwd().to_str().unwrap(), "/foo/bar/snerd");

        program.filename = Arc::new("../foo/bar/marf.c".into());
        assert_eq!(program.cwd().to_str().unwrap(), "/foo/bar");
    }

    #[test]
    fn relocate_moves_each_block_to_its_target() {
        use lpc_rs_asm::instruction::Instruction;
        use lpc_rs_core::lpc_type::LpcType;
        use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;
        use ustr::ustr;

        let region = |filename: &str, base, count| Region {
            filename: Arc::new(LpcPath::InGame(filename.into())),
            base,
            count,
            init: ustr(""),
        };
        let prototype = FunctionPrototypeBuilder::default()
            .name("f")
            .filename(Arc::new(LpcPath::InGame("/own.c".into())))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let mut function = ProgramFunction::new(prototype, 0);
        function.push_instruction(
            Instruction::Copy(
                RegisterVariant::Global(Register(1)),
                RegisterVariant::Global(Register(4)),
            ),
            None,
        );
        let mut functions: IndexMap<Ustr, Arc<ProgramFunction>, ahash::RandomState> =
            IndexMap::default();
        functions.insert(ustr("f"), Arc::new(function));
        let mut symbol = Symbol::new("g", LpcType::Int(false));
        symbol.location = Some(RegisterVariant::Global(Register(4)));
        let mut program = Program {
            functions: Box::new(functions),
            global_variables: Box::new(HashMap::from([("g".to_string(), symbol)])),
            layout: Box::new([region("/gp.c", 0, 3), region("/own.c", 3, 2)]),
            ..Program::default()
        };

        program.relocate_globals(&[10, 0]);

        assert_eq!(
            program.functions[&ustr("f")].instructions,
            [Instruction::Copy(
                RegisterVariant::Global(Register(11)),
                RegisterVariant::Global(Register(1)),
            )]
        );
        assert_eq!(
            program.global_variables["g"].location,
            Some(RegisterVariant::Global(Register(1)))
        );
        let bases: Vec<_> = program.layout.iter().map(|r| r.base).collect();
        assert_eq!(bases, [10, 0]);
    }

    fn program_with(name: &'static str) -> Program {
        let prototype = FunctionPrototypeBuilder::default()
            .name(name)
            .filename(Arc::new("/p.c".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let function = Arc::new(ProgramFunction::new(prototype, 0));
        let mut functions: IndexMap<Ustr, Arc<ProgramFunction>, ahash::RandomState> =
            IndexMap::default();
        functions.insert(ustr(&function.mangle()), function.clone());
        let mut unmangled: IndexMap<String, Arc<ProgramFunction>, ahash::RandomState> =
            IndexMap::default();
        unmangled.insert(name.to_string(), function);
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
