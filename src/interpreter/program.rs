use crate::semantic::program_function::ProgramFunction;
use lpc_rs_core::EFUN;
use rmp_serde::Serializer;
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::HashMap,
    fmt::{Display, Formatter},
    path::{Path, PathBuf},
    rc::Rc,
};

use itertools::Itertools;
use lpc_rs_core::{call_namespace::CallNamespace, INIT_PROGRAM};

use crate::{interpreter::efun::EFUN_PROTOTYPES, semantic::symbol::Symbol};
use serde::{Deserialize, Serialize};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_core::pragma_flags::PragmaFlags;

#[derive(Debug, Default, Serialize, Deserialize, PartialEq, Eq, Clone)]
pub struct Program {
    /// The path to the file that this program was compiled from. Used for error messaging.
    /// This is intended to be the fully-expanded, in-game path.
    pub filename: LpcPath,

    /// function mapping of name to Symbol
    pub functions: HashMap<String, Rc<ProgramFunction>>,

    /// The map of global variables in this program.
    /// Note this only includes vars defined within this program's file.
    pub global_variables: HashMap<String, Symbol>,

    /// How many globals does this program need storage for?
    pub num_globals: usize,

    /// How many [`Register`](lpc_rs_core::register::Register)s are needed to initialize this program?
    pub num_init_registers: usize,

    /// Which pragmas have been set for this program?
    pub pragmas: PragmaFlags,

    /// All of my Inherited parent objects
    /// The ordering of this field can be assumed to be in file order
    pub inherits: Vec<Program>,

    /// The index of name -> inherited objects, for inherits with names
    pub inherit_names: HashMap<String, usize>,
}

impl<'a> Program {
    pub fn new<T>(filename: T) -> Self
    where
        T: Into<LpcPath>,
    {
        Self {
            filename: filename.into(),
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
    /// and searching all of its inherited-from programs, last-declared-inherit first.
    pub fn lookup_function<T>(
        &self,
        name: T,
        namespace: &CallNamespace,
    ) -> Option<&Rc<ProgramFunction>>
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();

        let find_in_inherit = || {
            self.inherits
                .iter()
                .rev()
                .find_map(|p| p.lookup_function(r, &CallNamespace::Local))
        };

        match namespace {
            CallNamespace::Local => self.functions.get(r).or_else(find_in_inherit),
            CallNamespace::Parent => find_in_inherit(),
            CallNamespace::Named(ns) => self.inherit_names.get(ns).and_then(|i| {
                self.inherits
                    .get(*i)
                    .and_then(|p| p.lookup_function(name, &CallNamespace::Local))
            }),
        }
    }

    /// Return whether or not we have a function with this name either locally,
    /// or in any of our inherited-from parents.
    pub fn contains_function<T>(&self, name: T, namespace: &CallNamespace) -> bool
    where
        T: AsRef<str>,
    {
        let r = name.as_ref();

        let find_in_inherit = || {
            self.inherits
                .iter()
                .rev()
                .any(|p| p.contains_function(r, &CallNamespace::Local))
        };

        match namespace {
            CallNamespace::Local => self.functions.contains_key(r) || find_in_inherit(),
            CallNamespace::Parent => find_in_inherit(),
            CallNamespace::Named(ns) => match ns.as_str() {
                EFUN => EFUN_PROTOTYPES.contains_key(r),
                ns => self
                    .inherit_names
                    .get(ns)
                    .and_then(|i| {
                        self.inherits
                            .get(*i)
                            .map(|p| p.contains_function(name, &CallNamespace::Local))
                    })
                    .unwrap_or(false),
            },
        }
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
    /// use lpc_rs::compiler::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
    /// use lpc_rs::compiler::ast::int_node::IntNode;
    /// use lpc_rs::compiler::ast::expression_node::ExpressionNode;
    /// use lpc_rs::compiler::codegen::codegen_walker::CodegenWalker;
    /// use lpc_rs::compiler::codegen::tree_walker::TreeWalker;
    /// use lpc_rs::compiler::compilation_context::CompilationContext;
    /// use lpc_rs::compiler::Compiler;
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
    ///     .expect("Failed to compile.");
    ///
    /// for instruction in program.listing() {
    ///     println!("{}", instruction);
    /// }
    /// ```

    pub fn listing(&self) -> Vec<String> {
        let functions = self.functions.values().sorted_unstable_by(|a, b| {
            if a.name() == INIT_PROGRAM {
                return Ordering::Less;
            }
            if b.name() == INIT_PROGRAM {
                return Ordering::Greater;
            }

            Ord::cmp(&a.name(), &b.name())
        });

        functions.flat_map(|func| func.listing()).collect()
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

    use super::*;
    use crate::compiler::Compiler;

    #[test]
    fn test_serialization_and_deserialization() {
        let content = r#"
            int *foo = ({ 1, 2, 3, 4, 234 });
            void create() {
                foo = foo + ({ 666 });
                dump(foo);
            }
        "#;
        let compiler = Compiler::default();
        let prog = compiler
            .compile_string("foo.c", content.to_string())
            .unwrap();

        let msgpack = prog.to_msgpack();

        assert_eq!(Program::from(msgpack), prog);
    }

    #[test]
    fn test_cwd() {
        let mut program = Program {
            filename: "foo/bar/baz.c".into(),
            ..Program::default()
        };

        let full_path = Path::new(".").canonicalize().unwrap().display().to_string();
        assert_eq!(
            &*program.cwd().to_str().unwrap(),
            format!("{}/foo/bar", full_path)
        );

        program.filename = "marf.c".into();
        assert_eq!(&*program.cwd().to_str().unwrap(), full_path);

        program.filename = LpcPath::Server(Path::new("").to_path_buf());
        assert_eq!(&*program.cwd().to_str().unwrap(), "");
    }
}
