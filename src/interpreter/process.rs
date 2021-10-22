use crate::interpreter::{lpc_ref::LpcRef, program::Program};
use delegate::delegate;
use std::{
    borrow::Cow,
    cell::RefCell,
    fmt::{Display, Formatter},
    ops::Deref,
    path::Path,
    rc::Rc,
};

/// A wrapper type to allow the VM to keep the immutable `program` and its
/// mutable runtime pieces together.
#[derive(PartialEq, Eq, Debug, Default)]
pub struct Process {
    /// The [`Program`] that this process is running
    pub program: Rc<Program>,

    /// The stored global variable data for this instance
    pub globals: Vec<RefCell<LpcRef>>,

    /// What is the clone ID of this process? If `None`, this is a master object
    clone_id: Option<usize>,
}

impl Process {
    pub fn new<T>(prog: T) -> Self
    where
        T: Into<Rc<Program>>
    {
        let program = prog.into();
        let num_globals = program.num_globals;

        Self {
            program,
            globals: vec![RefCell::new(LpcRef::Int(0)); num_globals],
            clone_id: None,
        }
    }

    pub fn new_clone(program: Rc<Program>, clone_id: usize) -> Self {
        let num_globals = program.num_globals;

        Self {
            program,
            globals: vec![RefCell::new(LpcRef::Int(0)); num_globals],
            clone_id: Some(clone_id),
        }
    }

    delegate! {
        to self.program {
            /// Get the program's current working directory
            pub fn cwd(&self) -> Cow<'_, Path>;
        }
    }

    /// Get the filename of this process, including the clone ID suffix if present.
    #[inline]
    pub fn filename(&self) -> Cow<str> {
        let filename: &str = self.program.filename.as_ref();
        let name = filename.strip_suffix(".c").unwrap_or(filename);
        match self.clone_id {
            Some(x) => Cow::Owned(format!("{}#{}", name, x)),
            None => Cow::Borrowed(name),
        }
    }

    /// Get the filename with the passed prefix stripped off, defaulting to the `program` filename
    /// if that fails.
    #[inline]
    pub fn localized_filename(&self, prefix: &str) -> String {
        let filename: &str = &*self.filename();

        filename
            .strip_prefix(prefix)
            .unwrap_or_else(|| filename)
            .into()
    }
}

// This is an officially sanctioned abuse of Deref.
impl Deref for Process {
    type Target = Program;

    fn deref(&self) -> &Self::Target {
        &self.program
    }
}

impl Display for Process {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.filename())
    }
}

impl From<Process> for Rc<RefCell<Process>> {
    fn from(process: Process) -> Self {
        Rc::new(RefCell::new(process))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_filename() {
        let prog = Program {
            filename: "/foo/bar/baz.c".into(),
            ..Default::default()
        };
        let proc = Process::new(prog);
        assert_eq!(proc.filename(), "/foo/bar/baz");
    }

    #[test]
    fn test_localized_filename() {
        let prog = Program {
            filename: "/foo/bar/baz.c".into(),
            ..Default::default()
        };
        let proc = Process::new(prog);
        assert_eq!(proc.localized_filename(""), "/foo/bar/baz");

        assert_eq!(proc.localized_filename("/foo"), "/bar/baz");

        assert_eq!(proc.localized_filename("/alksdjf"), "/foo/bar/baz");
    }
}
