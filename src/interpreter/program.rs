use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Display, Formatter},
    path::{Path, PathBuf},
    sync::Arc,
};

use derive_builder::Builder;
use indexmap::IndexMap;
use itertools::Itertools;
use lpc_rs_core::{lpc_path::LpcPath, pragma_flags::PragmaFlags};
use lpc_rs_function_support::{program_function::ProgramFunction, symbol::Symbol};
use rmp_serde::Serializer;
use serde::{Deserialize, Serialize};
use string_interner::StringInterner;

#[derive(Debug, Default, Serialize, Deserialize, PartialEq, Eq, Clone, Builder)]
#[builder(default, build_fn(error = "lpc_rs_errors::LpcError"))]
pub struct Program {
    /// The path to the file that this program was compiled from.
    /// This is intended to be the fully-expanded, in-game path.
    #[builder(setter(into))]
    pub filename: Arc<LpcPath>,

    /// function mapping of (mangled) name to the function
    pub functions: IndexMap<String, Arc<ProgramFunction>>,

    /// Function mapping of unmangled name to the function.
    /// This is needed for `call_other`.
    /// Due to unmangled names not being unique, only the last-defined
    /// function with a given unmangled name is referenced here.
    pub unmangled_functions: IndexMap<String, Arc<ProgramFunction>>,

    /// The function that is called when the program is first loaded,
    /// which initializes the global variables. This function is
    /// the combined initializer of all of the inherited programs.
    pub initializer: Option<Arc<ProgramFunction>>,

    /// The map of global variables in this program.
    pub global_variables: HashMap<String, Symbol>,

    /// How many globals does this program need storage for?
    /// Note that this number includes inherited globals.
    pub num_globals: usize,

    /// How many [`Register`](lpc_rs_core::register::Register)s are needed to
    /// initialize this program?
    pub num_init_registers: usize,

    /// Which pragmas have been set for this program?
    pub pragmas: PragmaFlags,

    /// Interned strings
    #[builder(setter(into))]
    pub strings: Arc<StringInterner>,
}

impl<'a> Program {
    pub fn new<T>(filename: T) -> Self
    where
        T: Into<LpcPath>,
    {
        Self {
            filename: Arc::new(filename.into()),
            ..Default::default()
        }
    }

    /// Serialize the program to msgpack format, suitable for saving to disk.
    pub fn to_msgpack(&self) -> Vec<u8> {
        let mut buf = vec![];
        self.serialize(&mut Serializer::new(&mut buf)).unwrap();
        buf
    }

    /// Look up a function by its name, starting from this program,
    /// and searching all of its inherited-from programs, last-declared-inherit
    /// first.
    pub fn lookup_function<T>(&self, name: T) -> Option<&Arc<ProgramFunction>>
    where
        T: AsRef<str>,
    {
        let function_name = name.as_ref();
        self.functions
            .get(function_name)
            .or_else(|| self.unmangled_functions.get(function_name))
    }

    /// Return whether or not we have a function with this name either locally,
    /// or in any of our inherited-from parents.
    pub fn contains_function<T>(&self, name: T) -> bool
    where
        T: AsRef<str>,
    {
        let function_name = name.as_ref();
        self.functions.contains_key(function_name)
            || self.unmangled_functions.contains_key(function_name)
    }

    /// Call the passed callback, passing the function reference if found.
    pub fn with_function<F, T>(&self, name: &str, callback: F) -> Option<T>
    where
        F: FnOnce(&Arc<ProgramFunction>) -> T,
    {
        self.lookup_function(name).map(callback)
    }

    /// Get the directory of this program. Used for clone_object, etc.
    pub fn cwd(&'a self) -> Cow<'a, Path> {
        match self.filename.parent() {
            None => Cow::Owned(PathBuf::from("")),
            Some(path) => Cow::Borrowed(path),
        }
    }

    /// Get a listing of this Program's assembly language, suitable for printing
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::compiler::{
    ///     ast::{
    ///         binary_op_node::{BinaryOpNode, BinaryOperation},
    ///         expression_node::ExpressionNode,
    ///         int_node::IntNode,
    ///     },
    ///     codegen::{codegen_walker::CodegenWalker, tree_walker::TreeWalker},
    ///     compilation_context::CompilationContext,
    ///     Compiler,
    /// };
    /// use qcell::QCellOwner;
    ///
    /// let code = r#"
    ///     void foo() {
    ///         dump("sup?");
    ///     }
    /// "#;
    ///
    /// let compiler = Compiler::default();
    /// let mut cell_key = QCellOwner::new();
    /// let program = compiler
    ///     .compile_string("~/my_file.c", code, &mut cell_key)
    ///     .expect("Failed to compile.");
    ///
    /// for instruction in program.listing() {
    ///     println!("{}", instruction);
    /// }
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

impl From<Vec<u8>> for Program {
    /// Deserialize from msgpack data
    fn from(vec: Vec<u8>) -> Self {
        rmp_serde::from_slice(&vec).unwrap()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.filename)
    }
}

#[cfg(test)]
mod tests {
    use qcell::QCellOwner;

    use super::*;
    use crate::compiler::Compiler;

    #[test]
    fn test_serialization_and_deserialization() {
        let mut cell_key = QCellOwner::new();
        let content = r#"
            int *foo = ({ 1, 2, 3, 4, 234 });
            void create() {
                foo = foo + ({ 666 });
                dump(foo);
            }
        "#;
        let compiler = Compiler::default();
        let prog = compiler
            .compile_string("foo.c", content, &mut cell_key)
            .unwrap();

        let msgpack = prog.to_msgpack();

        assert_eq!(Program::from(msgpack), prog);
    }

    #[test]
    fn test_cwd() {
        let mut program = Program {
            filename: Arc::new("foo/bar/baz.c".into()),
            ..Program::default()
        };

        let full_path = Path::new(".").canonicalize().unwrap().display().to_string();
        assert_eq!(
            program.cwd().to_str().unwrap(),
            format!("{full_path}/foo/bar")
        );

        program.filename = Arc::new("marf.c".into());
        assert_eq!(program.cwd().to_str().unwrap(), full_path);

        program.filename = Arc::new(LpcPath::Server(Path::new("").to_path_buf()));
        assert_eq!(program.cwd().to_str().unwrap(), "");
    }
}
