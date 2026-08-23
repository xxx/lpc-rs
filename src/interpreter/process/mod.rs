pub mod util;

use std::{
    borrow::Cow,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    sync::{Arc, OnceLock},
};

use bit_set::BitSet;
use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    interpreter::{
        gc::mark::Mark,
        lpc_array::LpcArray,
        lpc_ref::LpcRef,
        process::util::AllEnvironment,
        program::Program,
        stm::{SVar, Transaction, TxnHandle, VarId, WorldValue},
    },
    telnet::connection::Connection,
};

#[derive(Debug)]
/// A type to represent the position of a [`Process`] in the game world.
///
/// Both slots are transactional cells: the process holds only the cell
/// identities, and the committed values live in the committer's world. A
/// concurrent `move_object` therefore conflicts at commit and re-runs, instead
/// of mutating shared state under a semaphore.
pub struct ProcessPosition {
    /// The cell holding the object that contains this object
    /// (`LpcRef::Object`, absent when top-level).
    pub environment: SVar<LpcRef>,

    /// The cell holding the objects that this object contains: an array
    /// payload of `LpcRef::Object`, one per contained object.
    pub inventory: SVar<LpcArray>,
}

impl Default for ProcessPosition {
    fn default() -> Self {
        Self {
            environment: SVar::new(),
            inventory: SVar::new(),
        }
    }
}

/// A wrapper type to allow the VM to keep the immutable `program` and its
/// mutable runtime pieces together.
#[derive(Debug)]
pub struct Process {
    /// The [`Program`] that this process is running.
    pub program: Arc<Program>,

    /// One slot per program global; fixed size at construction. The slot is a
    /// pure identity cell (a `VarId`); the *committed value* lives only in
    /// the committer's world.
    globals: Box<[SVar<LpcRef>]>,

    /// What is the clone ID of this process? If `None`, this is a master
    /// object.
    clone_id: Option<usize>,

    /// The player [`Connection`] that this [`Process`] is associated with, if any.
    /// A transactional cell: an `exec` or login writes the cell in its own
    /// transaction (so the rest of that task sees the binding) and records a
    /// deferred socket-level handover, while the [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker)
    /// keeps the physical `Connection`. `interactive()`/`this_player()` read
    /// the cell through the transaction, so they see an in-transaction `exec`.
    pub connection: SVar<Connection>,

    /// The cell marking that the initializer has run; a rejected
    /// initialization rolls it back with the globals.
    pub initialized: SVar<LpcRef>,

    /// The cell marking that commands are enabled; absent means disabled.
    pub commands_enabled: SVar<LpcRef>,

    /// The object-space cell this process lives under, known once it is
    /// inserted or destructed transactionally.
    pub cell: OnceLock<VarId>,

    /// Where are we in the game world?
    pub position: ProcessPosition,
}

/// A `Process` with an empty `Program`: no globals, no clone id, no
/// connection, default flags and position. Test fixtures use this.
impl Default for Process {
    fn default() -> Self {
        Self {
            program: Arc::default(),
            globals: Vec::new().into_boxed_slice(),
            clone_id: None,
            connection: SVar::new(),
            initialized: SVar::new(),
            commands_enabled: SVar::new(),
            cell: OnceLock::new(),
            position: Default::default(),
        }
    }
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
            globals: (0..num_globals as usize)
                .map(|_| SVar::new())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            clone_id: None,
            connection: SVar::new(),
            initialized: SVar::new(),
            commands_enabled: SVar::new(),
            cell: OnceLock::new(),
            position: Default::default(),
        }
    }

    /// Create a new [`Process`] from the passed [`Program`], with the passed
    /// clone ID.
    pub fn new_clone(program: Arc<Program>, clone_id: usize) -> Self {
        let num_globals = program.num_globals;

        Self {
            program,
            globals: (0..num_globals as usize)
                .map(|_| SVar::new())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            clone_id: Some(clone_id),
            connection: SVar::new(),
            initialized: SVar::new(),
            commands_enabled: SVar::new(),
            cell: OnceLock::new(),
            position: Default::default(),
        }
    }

    /// Returns an iterator over all of `object`'s environments, starting with
    /// its current one, following the chain through the passed transaction.
    pub(crate) fn all_environment(txn: TxnHandle, object: Arc<Process>) -> AllEnvironment {
        AllEnvironment::new(txn, object)
    }

    /// The committed environment of `process` as seen through `txn`, if any.
    pub(crate) fn environment_of(txn: &TxnHandle, process: &Arc<Process>) -> Option<Arc<Process>> {
        txn.with(|t| {
            t.read(process.position.environment.id)
                .and_then(|value| match value {
                    LpcRef::Object(weak) => weak.upgrade(),
                    _ => None,
                })
        })
    }

    /// The committed inventory of `process` as seen through `txn`, destructed
    /// members filtered out.
    pub(crate) fn inventory_of(txn: &TxnHandle, process: &Arc<Process>) -> Vec<Arc<Process>> {
        txn.with(|t| {
            let Some(WorldValue::Array(inventory)) = t.read_value(process.position.inventory.id)
            else {
                return Vec::new();
            };

            inventory
                .iter()
                .filter_map(|item| match item {
                    LpcRef::Object(weak) => weak.upgrade(),
                    _ => None,
                })
                .collect()
        })
    }

    /// Move `object` into `new_environment`, through the caller's transaction.
    ///
    /// A pure read-modify-write over the position cells: the old and new
    /// environments' inventories plus the object's environment pointer are
    /// read (tracked) and written into the in-flight changeset. No physical
    /// mutation, no locking — concurrent moves to the same environment
    /// conflict at commit and the loser re-runs.
    pub(crate) fn move_to(txn: &TxnHandle, object: &Arc<Process>, new_environment: &Arc<Process>) {
        let object_cell = object.position.environment.id;
        let new_cell = new_environment.position.inventory.id;
        let new_env_ref = LpcRef::Object(Arc::downgrade(new_environment));
        let new_member = LpcRef::Object(Arc::downgrade(object));

        txn.with(|t| {
            let current_env = Self::environment_of_inner(t, object);

            // Moving to the current environment is a no-op; without the check
            // the same reference would be added to the inventory twice.
            if current_env
                .as_ref()
                .is_some_and(|env| std::ptr::eq(env.as_ref(), new_environment.as_ref()))
            {
                return;
            }

            // Remove from the current environment's inventory, if it has one.
            if let Some(old) = current_env {
                t.with_array_cow(old.position.inventory.id, |inventory| {
                    inventory
                        .array
                        .retain(|item| !Self::is_same_object(item, object));
                });
            }

            // Add to the new environment's inventory and point the object at it.
            t.with_array_cow(new_cell, |inventory| {
                inventory.array.push(new_member);
            });
            t.write(object_cell, new_env_ref);
        });
    }

    /// The environment of `process` as seen inside `t` (an in-flight
    /// transaction), if any.
    fn environment_of_inner(t: &mut Transaction, process: &Arc<Process>) -> Option<Arc<Process>> {
        t.read(process.position.environment.id).and_then(|value| {
            let LpcRef::Object(weak) = value else {
                return None;
            };
            weak.upgrade()
        })
    }

    /// Whether an inventory slot holds `object`, compared by referent because
    /// the cells' `Weak` handles are distinct allocations.
    fn is_same_object(slot: &LpcRef, object: &Arc<Process>) -> bool {
        let LpcRef::Object(weak) = slot else {
            return false;
        };

        weak.upgrade()
            .is_some_and(|upgraded| std::ptr::eq(upgraded.as_ref(), object.as_ref()))
    }

    /// Whether the initializer has run, read through `txn` and tracked.
    /// Whether this object is still live for `txn`: not removed by a
    /// committed destruct, nor by this attempt.
    pub(crate) fn is_live(&self, txn: &TxnHandle) -> bool {
        !self
            .cell
            .get()
            .is_some_and(|&cell| txn.with(|t| t.is_removed(cell)))
    }

    pub(crate) fn is_initialized(&self, txn: &TxnHandle) -> bool {
        txn.with(|t| t.read(self.initialized.id).is_some())
    }

    /// Write the marker in `txn` and return `true`, or `false` when already
    /// initialized.
    pub(crate) fn claim_init(&self, txn: &TxnHandle) -> bool {
        txn.with(|t| {
            if t.read(self.initialized.id).is_some() {
                return false;
            }
            t.write(self.initialized.id, LpcRef::from(1));
            true
        })
    }

    /// Whether this process was made by `clone_object`.
    pub fn is_clone(&self) -> bool {
        self.clone_id.is_some()
    }

    /// Whether `enable_commands()` is in effect, as seen through `txn`.
    pub(crate) fn commands_enabled(&self, txn: &TxnHandle) -> bool {
        txn.with(|t| t.read(self.commands_enabled.id).is_some())
    }

    /// The committer-world identity of a global slot.
    pub(crate) fn var_id(&self, reg: RegisterSize) -> VarId {
        self.globals[reg as usize].id
    }

    /// The world ids a live object keeps alive, rooted even when the object
    /// has no committed `Process` cell.
    pub(crate) fn world_var_ids(&self) -> Vec<VarId> {
        self.globals
            .iter()
            .map(|slot| slot.id)
            .chain([
                self.initialized.id,
                self.commands_enabled.id,
                self.position.environment.id,
                self.position.inventory.id,
                self.connection.id,
            ])
            .collect()
    }

    /// Get the filename of this process, including the clone ID suffix if
    /// present.
    #[inline]
    pub fn filename(&self) -> Cow<'_, str> {
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

impl Display for Process {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.filename())
    }
}

impl Mark for Process {
    fn mark(&self, _marked: &mut BitSet, _processed: &mut BitSet) -> Result<()> {
        // The slot identities own no values. Live values are transaction
        // roots, marked through the task's in-flight changeset;
        // committed values live in the committer's world.
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;

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
}
