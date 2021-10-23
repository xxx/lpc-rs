use crate::{interpreter::pragma_flags::PragmaFlags, semantic::program_function::ProgramFunction};
use rmp_serde::Serializer;
use serde::Serialize;
use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Display, Formatter},
    path::{Path, PathBuf},
    rc::Rc,
};
use crate::util::path_maker::LpcPath;

#[derive(Debug, Default, Serialize, Deserialize, PartialEq, Eq, Clone)]
pub struct Program {
    /// The path to the file that this program was compiled from. Used for error messaging.
    /// This is intended to be the fully-expanded, in-game path.
    pub filename: LpcPath,

    /// function mapping of name to Symbol
    pub functions: HashMap<String, Rc<ProgramFunction>>,

    /// How many globals does this program need storage for?
    pub num_globals: usize,

    /// How many [`Register`]s are needed to initialize this program?
    pub num_init_registers: usize,

    /// Which pragmas have been set for this program?
    pub pragmas: PragmaFlags,
}

impl<'a> Program {
    /// Serialize the program to msgpack format, suitable for saving to disk.
    pub fn to_msgpack(&self) -> Vec<u8> {
        let mut buf = vec![];
        self.serialize(&mut Serializer::new(&mut buf)).unwrap();
        buf
    }

    /// Look up a function by its name
    pub fn lookup_function<T>(&self, name: T) -> Option<&Rc<ProgramFunction>>
    where
        T: AsRef<str>,
    {
        self.functions.get(name.as_ref())
    }

    /// Get the directory of this program. Used for clone_object, etc.
    pub fn cwd(&'a self) -> Cow<'a, Path> {
        match self.filename.parent() {
            None => Cow::Owned(PathBuf::from("")),
            Some(path) => Cow::Borrowed(path),
        }
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
    use crate::util::config::Config;

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
        assert_eq!(&*program.cwd().to_str().unwrap(), format!("{}/foo/bar", full_path));

        program.filename = "marf.c".into();
        assert_eq!(&*program.cwd().to_str().unwrap(), full_path);

        program.filename = LpcPath::Server(Path::new("").to_path_buf());
        assert_eq!(&*program.cwd().to_str().unwrap(), "");
    }
}
