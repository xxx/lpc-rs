pub mod util;

use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    path::Path,
    sync::{Arc, Weak},
};

use arc_swap::ArcSwapAny;
use bit_set::BitSet;
use crossbeam::atomic::AtomicCell;
use dashmap::DashSet;
use delegate::delegate;
use educe::Educe;
use if_chain::if_chain;
use lpc_rs_errors::{lpc_error, Result};
use parking_lot::RwLock;
use sharded_slab::Slab as ShardedSlab;
use tokio::sync::Semaphore;

use crate::{
    interpreter::{
        bank::RefBank,
        gc::mark::Mark,
        lpc_ref::{LpcRef, NULL},
        object_flags::{AtomicFlags, ObjectFlags},
        process::util::AllEnvironment,
        program::Program,
    },
    telnet::connection::Connection,
};

#[derive(Educe)]
#[educe(Debug)]
/// A type to represent the position of a [`Process`] in the game world.
pub struct ProcessPosition {
    /// The object that contains this object. This object is in that object's `inventory`.
    pub environment: ArcSwapAny<Option<Weak<Process>>>,

    /// The objects that this object contains. This object is the `environment` for everything in this container.
    #[educe(Debug(ignore))]
    pub inventory: ShardedSlab<Weak<Process>>,

    /// The inventory IDs of this object's inventory. This is needed for iteration.
    pub inventory_ids: DashSet<usize>,

    /// The inventory ID of this object in its environment. This is the index into our `environment`'s
    /// `inventory` [`Slab`]. Needed for removal.
    environment_inventory_id: AtomicCell<usize>,

    /// The semaphore that prevents multiple threads from moving this object simultaneously, since it
    /// needs to happen in a transactional manner. The semaphore is specifically for when this object
    /// is being moved. The new and former environments can use standard locking.
    move_semaphore: Semaphore,
}

impl ProcessPosition {
    pub fn environment_inventory_id(&self) -> usize {
        self.environment_inventory_id.load()
    }

    pub fn weak_inventory_iter(&self) -> impl Iterator<Item = Weak<Process>> + '_ {
        self.inventory_ids
            .iter()
            .filter_map(move |id| self.inventory.get(*id))
            .map(|x| x.clone())
    }

    pub fn inventory_iter(&self) -> impl Iterator<Item = Arc<Process>> + '_ {
        self.inventory_ids
            .iter()
            .filter_map(move |id| self.inventory.get(*id))
            .filter_map(|weak| weak.upgrade())
    }
}

impl Default for ProcessPosition {
    fn default() -> Self {
        ProcessPosition {
            environment: ArcSwapAny::from(None),
            inventory: Default::default(),
            inventory_ids: Default::default(),
            environment_inventory_id: Default::default(),
            move_semaphore: Semaphore::new(1),
        }
    }
}

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
    /// The [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker)
    /// owns the [`Connection`] with the [`Process`] set on it.
    pub connection: ArcSwapAny<Option<Arc<Connection>>>,

    /// Our flags
    pub flags: AtomicFlags<ObjectFlags>,

    /// Where are we in the game world?
    pub position: ProcessPosition,
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
            globals: RwLock::new(RefBank::new(vec![NULL; num_globals as usize])),
            clone_id: None,
            connection: ArcSwapAny::from(None),
            flags: Default::default(),
            position: Default::default(),
        }
    }

    /// Create a new [`Process`] from the passed [`Program`], with the passed
    /// clone ID.
    pub fn new_clone(program: Arc<Program>, clone_id: usize) -> Self {
        let num_globals = program.num_globals;

        let flags = AtomicFlags::new();
        flags.set(ObjectFlags::Clone);

        Self {
            program,
            globals: RwLock::new(RefBank::new(vec![NULL; num_globals as usize])),
            clone_id: Some(clone_id),
            connection: ArcSwapAny::from(None),
            flags,
            position: Default::default(),
        }
    }

    delegate! {
        to self.program {
            /// Get the program's current working directory
            pub fn cwd(&self) -> Cow<'_, Path>;
        }
    }

    /// Returns an iterator over all of `object`'s environments, starting with their current environment.
    pub fn all_environment(object: Arc<Process>) -> AllEnvironment {
        AllEnvironment::new(object)
    }

    /// Move an object to a new environment. This is a transactional operation, so it will
    /// block until it can acquire the semaphore.
    pub async fn move_to(object: &Arc<Process>, new_environment: Arc<Process>) -> Result<()> {
        let current_env = if_chain! {
            if let Some(current_env) = &*object.position.environment.load();
            if let Some(current_env) = current_env.upgrade();
            then {
                if current_env == new_environment {
                    return Ok(())
                }

                Some(current_env)
            } else {
                None
            }
        };

        // The moving object needs to be locked for the rest of the move.
        let _object_permit = object.position.move_semaphore.acquire().await;

        // old_env.inventory -= ob
        if let Some(old_environment) = current_env {
            // Take the old environment's lock. We remove all object data from it in this block.
            let _old_permit = old_environment.position.move_semaphore.acquire().await;

            let current_inventory_id = object.position.environment_inventory_id.load();

            old_environment
                .position
                .inventory
                .remove(current_inventory_id);
            old_environment
                .position
                .inventory_ids
                .remove(&current_inventory_id);
        }

        let new_env_weak = Arc::downgrade(&new_environment);

        // new_env.inventory += ob
        {
            // Take the new environment's lock. We add all object data to it in this block.
            let _new_permit = new_environment.position.move_semaphore.acquire().await;

            let Some(entry) = new_environment.position.inventory.vacant_entry() else {
                return Err(
                    lpc_error!("new environment is full. cannot move. this really only happens in an out-of-memory situation, so I'm not bothering to clean up.")
                );
            };

            new_environment.position.inventory_ids.insert(entry.key());
            object.position.environment_inventory_id.store(entry.key());

            // new_env.inventory += ob
            entry.insert(Arc::downgrade(object));
        }

        // ob.environment = new_env
        object.position.environment.store(Some(new_env_weak));

        Ok(())
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
        self.filename() == other.filename()
        // Arc::ptr_eq(&self.program, &other.program) && self.clone_id == other.clone_id
    }
}

impl Eq for Process {}

impl Hash for Process {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        // NOTE: this should not be based on any field with interior mutability. It's used
        // to prevent infinite looping in numerous places.
        self.filename().as_ref().hash(state)
    }
}

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
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
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

        assert!(processed.contains(*array_id.as_ref() as usize));
    }
}
