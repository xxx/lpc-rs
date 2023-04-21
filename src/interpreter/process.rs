use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    path::Path,
    sync::Arc,
};

use bit_set::BitSet;
use delegate::delegate;
use parking_lot::RwLock;

use crate::{
    interpreter::{
        bank::RefBank,
        gc::mark::Mark,
        lpc_ref::{LpcRef, NULL},
        program::Program,
    },
    telnet::connection::Connection,
};

/// A wrapper type to allow the VM to keep the immutable `program` and its
/// mutable runtime pieces together.
#[derive(Debug, Default)]
pub struct Process {
    /// The [`Program`] that this process is running.
    pub program: Arc<Program>,

    /// The stored global variable data for this instance.
    pub globals: RwLock<RefBank>,

    /// What is the clone ID of this process? If `None`, this is a master
    /// object.
    clone_id: Option<usize>,

    /// The player [`Connection`] that this [`Process`] is associated with, if any.
    /// *Note*: This connection will typically never have the [`Process`] set on it,
    /// and should be assumed to be `None` at all times.
    /// The [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker)
    /// owns the [`Connection`] with the [`Process`] set on it.
    pub connection: RwLock<Option<Connection>>,
}

impl Process {
    /// Create a new [`Process`] from the passed [`Program`].
    pub fn new<T>(prog: T) -> Self
    where
        T: Into<Arc<Program>>,
    {
        let program = prog.into();
        let num_globals = program.num_globals;

        Self {
            program,
            globals: RwLock::new(RefBank::new(vec![NULL; num_globals])),
            clone_id: None,
            connection: RwLock::new(None),
        }
    }

    /// Create a new [`Process`] from the passed [`Program`], with the passed
    /// clone ID.
    pub fn new_clone(program: Arc<Program>, clone_id: usize) -> Self {
        let num_globals = program.num_globals;

        Self {
            program,
            globals: RwLock::new(RefBank::new(vec![NULL; num_globals])),
            clone_id: Some(clone_id),
            connection: RwLock::new(None),
        }
    }

    delegate! {
        to self.program {
            /// Get the program's current working directory
            pub fn cwd(&self) -> Cow<'_, Path>;
        }
    }

    /// Get a HashMap of global variable names to their current values
    pub fn global_variable_values(&self) -> HashMap<&str, LpcRef> {
        self.program
            .global_variables
            .iter()
            .filter_map(|(k, v)| {
                let value = &self.globals.read()[v.location?.index()];
                Some((k.as_str(), value.clone()))
            })
            .collect()
    }

    /// Get the filename of this process, including the clone ID suffix if
    /// present.
    #[inline]
    pub fn filename(&self) -> Cow<str> {
        let filename: &str = (*self.program.filename).as_ref();
        let name = filename.strip_suffix(".c").unwrap_or(filename);
        match self.clone_id {
            Some(x) => Cow::Owned(format!("{name}#{x}")),
            None => Cow::Borrowed(name),
        }
    }

    /// Get the filename with the passed prefix stripped off, defaulting to the
    /// `program` filename if that fails.
    #[inline]
    pub fn localized_filename(&self, prefix: &str) -> String {
        let filename: &str = &self.filename();

        filename.strip_prefix(prefix).unwrap_or(filename).into()
    }
}

impl PartialEq for Process {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.program, &other.program) && self.clone_id == other.clone_id
    }
}

impl Eq for Process {}

impl AsRef<Program> for Process {
    #[inline]
    fn as_ref(&self) -> &Program {
        &self.program
    }
}

impl Display for Process {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.filename())
    }
}

impl Mark for Process {
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> lpc_rs_errors::Result<()> {
        self.globals.read().mark(marked, processed)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::interpreter::{heap::Heap, into_lpc_ref::IntoLpcRef, lpc_array::LpcArray};

    #[test]
    fn test_filename() {
        let prog = Program {
            filename: Arc::new("/foo/bar/baz.c".into()),
            ..Default::default()
        };
        let proc = Process::new(prog);
        assert_eq!(proc.filename(), "/foo/bar/baz");
    }

    #[test]
    fn test_localized_filename() {
        let prog = Program {
            filename: Arc::new("/foo/bar/baz.c".into()),
            ..Default::default()
        };
        let proc = Process::new(prog);
        assert_eq!(proc.localized_filename(""), "/foo/bar/baz");

        assert_eq!(proc.localized_filename("/foo"), "/bar/baz");

        assert_eq!(proc.localized_filename("/alksdjf"), "/foo/bar/baz");
    }

    #[test]
    fn test_mark() {
        let memory = Heap::new(5);
        let array = LpcArray::new(vec![]);
        let array_id = array.unique_id;
        let lpc_ref = array.into_lpc_ref(&memory);

        let process = Process::default();
        process.globals.write().push(lpc_ref);

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        process.mark(&mut marked, &mut processed).unwrap();

        assert!(processed.contains(*array_id.as_ref()));
    }
}
