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
use lpc_rs_core::{
    RegisterSize,
    lpc_path::LpcPath,
    pragma_flags::PragmaFlags,
    register::{Register, RegisterVariant},
};
use lpc_rs_function_support::{program_function::ProgramFunction, symbol::Symbol};
use path_dedot::*;

#[derive(Debug, Default, PartialEq, Eq, Clone, Builder)]
#[builder(default, build_fn(error = "lpc_rs_errors::LpcError"))]
pub struct Program {
    /// The path to the file that this program was compiled from.
    /// This is intended to be the fully-expanded, in-game path.
    #[builder(setter(into))]
    pub filename: Arc<LpcPath>,

    /// function mapping of (mangled) name to the function
    pub functions: Box<IndexMap<String, Arc<ProgramFunction>>>,

    /// Function mapping of unmangled name to the function.
    /// This is needed for `call_other`.
    /// Due to unmangled names not being unique, only the last-defined
    /// function with a given unmangled name is referenced here.
    pub unmangled_functions: Box<IndexMap<String, Arc<ProgramFunction>>>,

    /// The function that is called when the program is first loaded,
    /// which initializes the global variables. This function is
    /// the combined initializer of all of the inherited programs.
    pub initializer: Option<Arc<ProgramFunction>>,

    /// The map of global variables in this program.
    pub global_variables: Box<HashMap<String, Symbol>>,

    /// How many globals does this program need storage for?
    /// Note that this number includes inherited globals.
    pub num_globals: RegisterSize,

    /// Which pragmas have been set for this program?
    pub pragmas: PragmaFlags,
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

    /// Get the in-game directory of this program. Used for clone_object, etc.
    pub fn cwd(&'a self) -> Cow<'a, Path> {
        match self.filename.parent() {
            None => Cow::Owned(PathBuf::from("/")),
            Some(path) => {
                let dedotted = path.parse_dot_from("/");

                if path.is_absolute() {
                    dedotted
                } else {
                    Cow::Owned(PathBuf::from("/").join(dedotted))
                }
            }
        }
    }

    /// Move every global this program owns up by `base`, so it can be
    /// imported into a child whose earlier parents hold the first `base` slots.
    pub fn rebase_globals(&mut self, base: RegisterSize) {
        if base == 0 {
            return;
        }
        for func in self.functions.values_mut() {
            let mut shifted = ProgramFunction::clone(func);
            for instruction in &mut shifted.instructions {
                *instruction = instruction.shift_globals(base);
            }
            *func = Arc::new(shifted);
        }
        for symbol in self.global_variables.values_mut() {
            if let Some(RegisterVariant::Global(reg)) = symbol.location {
                symbol.location = Some(RegisterVariant::Global(Register(reg.index() + base)));
            }
        }
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
    ///     .expect("Failed to compile.");
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
}
