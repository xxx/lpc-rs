pub(crate) mod cleanup;
mod image;
pub(crate) mod shadow;
pub mod util;

pub(crate) use image::ProgramImage;
pub use shadow::ShadowLinks;

use std::{
    borrow::Cow,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    sync::{
        Arc, OnceLock,
        atomic::{AtomicBool, Ordering},
    },
};

use lpc_rs_core::lpc_path::{LibRoot, LpcPath};

use crate::{
    command::registry::RuleList,
    interpreter::{
        lpc_array::LpcArray,
        lpc_ref::LpcRef,
        process::util::AllEnvironment,
        program::{Program, in_game_dir},
        stm::{MergeOp, SVar, Transaction, TxnHandle, VarId, WorldValue},
    },
    telnet::connection::Connection,
};

#[derive(Debug, Default)]
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

    /// The living objects among this object's contents.
    pub livings: SVar<LpcArray>,
}

/// How a process is named: what `file_name` answers and the object space
/// keys on.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectName {
    /// The object of a source file: the program's filename, `.c` stripped.
    File,
    /// `clone_object`'s product: the program's filename plus `#<id>`.
    Clone(usize),
    /// `compile_object`'s product: the requested path, an object key.
    Virtual(String),
}

/// An object's stable identity and transactional runtime bindings.
#[derive(Debug)]
pub struct Process {
    initial_image: Arc<ProgramImage>,

    /// The transaction-visible program and global-cell layout after an upgrade.
    pub(crate) image_cell: SVar<ProgramImage>,

    /// The second this process was constructed: what `object_time` answers.
    pub created: i64,

    /// Present only for nonresident programs defining the idle cleanup apply.
    pub(crate) cleanup: Option<Box<cleanup::Cleanup>>,

    /// How this process is named.
    name: ObjectName,

    /// The player [`Connection`] that this [`Process`] is associated with, if any.
    /// A transactional cell: an `exec` or login writes the cell in its own
    /// transaction (so the rest of that task sees the binding) and records a
    /// deferred socket-level handover, while the connection's own
    /// back-reference is the loop's view. `interactive()`/`this_player()`
    /// read the cell through the transaction, so they see an in-transaction
    /// `exec`. The cell is written by the attach/`exec`/destruct
    /// transactions; the connection's back-reference only by the binding
    /// module.
    pub connection: SVar<Connection>,

    /// Allocated only when this object participates in snooping.
    pub(crate) snoop: OnceLock<Box<crate::interpreter::snooping::Slots>>,

    /// Allocated only when this object's native TCP inventory is accessed.
    pub(crate) socket_index: OnceLock<VarId>,

    /// The cell marking that the initializer has run; a rejected
    /// initialization rolls it back with the globals.
    pub initialized: SVar<LpcRef>,

    /// The cell marking that commands are enabled; absent means disabled.
    pub commands_enabled: SVar<LpcRef>,

    /// The cell holding what this object can command; absent means no rules.
    pub rules: SVar<RuleList>,

    /// Set by `parse_init()`; `parse_add_rule` requires it. Not
    /// transactional: setting it twice is harmless.
    pub parser_ready: OnceLock<()>,

    /// The object-space cell this process lives under, known once it is
    /// inserted or destructed transactionally.
    pub cell: OnceLock<VarId>,

    /// Physical bootstrap objects may have no value in their path cell.
    pub(crate) physical: AtomicBool,

    /// Set under the call-out queue lock when committed destruction cancels this owner's timers.
    pub(crate) call_outs_retired: AtomicBool,

    /// Where are we in the game world?
    pub position: ProcessPosition,

    /// Who shadows this object, and what it shadows.
    pub shadow: ShadowLinks,
}

/// A `Process` with an empty `Program`: no globals, an `ObjectName::File`
/// name, no connection, default flags and position. Test fixtures use this.
impl Default for Process {
    fn default() -> Self {
        Self {
            initial_image: Arc::new(ProgramImage::new(Arc::default())),
            image_cell: SVar::new(),
            created: 0,
            cleanup: None,
            name: ObjectName::File,
            connection: SVar::new(),
            snoop: OnceLock::new(),
            socket_index: OnceLock::new(),
            initialized: SVar::new(),
            commands_enabled: SVar::new(),
            rules: SVar::new(),
            parser_ready: OnceLock::new(),
            cell: OnceLock::new(),
            physical: AtomicBool::new(false),
            call_outs_retired: AtomicBool::new(false),
            position: Default::default(),
            shadow: Default::default(),
        }
    }
}

/// Where an object stands for one attempt.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Liveness {
    /// Removed by a committed destruct or by this attempt.
    Dead,
    /// Resident, its initializer not yet run.
    Uninitialized,
    /// Resident and initialized.
    Ready,
}

impl Process {
    pub(crate) fn storage_bytes(&self, image: &ProgramImage) -> lpc_rs_errors::Result<usize> {
        let mut size = crate::interpreter::memory::Size::new(size_of::<Self>());
        size.items::<usize>(2)?;
        size.add(image.storage_bytes()?)?;
        if let ObjectName::Virtual(name) = &self.name {
            size.add(name.capacity())?;
        }
        Ok(size.bytes())
    }

    /// Shared constructor body for `new`, `new_clone` and `new_virtual`.
    fn with_name(program: Arc<Program>, name: ObjectName) -> Self {
        Self {
            cleanup: (!program.pragmas.resident()
                && program
                    .unmangled_functions
                    .contains_key(crate::interpreter::CLEAN_UP))
            .then(Box::default),
            initial_image: Arc::new(ProgramImage::new(program)),
            image_cell: SVar::new(),
            created: chrono::Utc::now().timestamp(),
            name,
            connection: SVar::new(),
            snoop: OnceLock::new(),
            socket_index: OnceLock::new(),
            initialized: SVar::new(),
            commands_enabled: SVar::new(),
            rules: SVar::new(),
            parser_ready: OnceLock::new(),
            cell: OnceLock::new(),
            physical: AtomicBool::new(false),
            call_outs_retired: AtomicBool::new(false),
            position: Default::default(),
            shadow: Default::default(),
        }
    }

    /// Create a new [`Process`] from the passed [`Program`].
    pub fn new<T>(prog: T) -> Self
    where
        T: Into<Arc<Program>>,
    {
        Self::with_name(prog.into(), ObjectName::File)
    }

    /// Create a new [`Process`] from the passed [`Program`], with the passed
    /// clone ID.
    pub fn new_clone(program: Arc<Program>, clone_id: usize) -> Self {
        Self::with_name(program, ObjectName::Clone(clone_id))
    }

    /// `program`'s code under `name`, an object key such as
    /// `/inst/17/d/room1`: what `compile_object` produces.
    pub fn new_virtual(program: Arc<Program>, name: String) -> Self {
        Self::with_name(program, ObjectName::Virtual(name))
    }

    /// The construction-time program, for compiler and bootstrap inspection only.
    pub fn initial_program(&self) -> &Arc<Program> {
        &self.initial_image.program
    }

    pub(crate) fn initial_image(&self) -> &Arc<ProgramImage> {
        &self.initial_image
    }

    pub(crate) fn image_in(&self, txn: &mut Transaction) -> Arc<ProgramImage> {
        match txn.read_value(self.image_cell.id) {
            Some(WorldValue::Image(image)) => image,
            None => self.initial_image.clone(),
            _ => unreachable!("a process image cell holds an image"),
        }
    }

    pub(crate) fn image(&self, txn: &TxnHandle) -> Arc<ProgramImage> {
        txn.with(|txn| self.image_in(txn))
    }

    /// The program selected by this transaction, including a staged upgrade.
    pub(crate) fn program(&self, txn: &TxnHandle) -> Arc<Program> {
        self.image(txn).program.clone()
    }

    pub(crate) fn is_prototype(&self) -> bool {
        matches!(self.name, ObjectName::File)
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

    /// The live inventory of `process` as seen through `txn`, in arrival order.
    pub(crate) fn inventory_of(txn: &TxnHandle, process: &Arc<Process>) -> Vec<Arc<Process>> {
        Self::objects_in(txn, process.position.inventory.id)
    }

    /// `root`, then its contents depth-first — a container's contents right
    /// behind it — each object once.
    pub(crate) fn deep_inventory_of(txn: &TxnHandle, root: &Arc<Process>) -> Vec<Arc<Process>> {
        let mut out = vec![root.clone()];
        let mut stack = vec![Self::inventory_of(txn, root).into_iter()];
        while let Some(top) = stack.last_mut() {
            let Some(item) = top.next() else {
                stack.pop();
                continue;
            };
            if out.iter().any(|seen| Arc::ptr_eq(seen, &item)) {
                continue;
            }
            out.push(item.clone());
            stack.push(Self::inventory_of(txn, &item).into_iter());
        }
        out
    }

    /// The live command-enabled members of `process`'s contents as seen through `txn`.
    pub(crate) fn livings_of(txn: &TxnHandle, process: &Arc<Process>) -> Vec<Arc<Process>> {
        Self::objects_in(txn, process.position.livings.id)
    }

    /// The live objects an array cell names, as seen through `txn`.
    fn objects_in(txn: &TxnHandle, var_id: VarId) -> Vec<Arc<Process>> {
        txn.with(|t| {
            let Some(WorldValue::Array(members)) = t.read_value(var_id) else {
                return Vec::new();
            };

            members
                .iter()
                .filter_map(|item| match item {
                    LpcRef::Object(weak) => weak.upgrade(),
                    _ => None,
                })
                .filter(|object| object.is_live_in(t))
                .collect()
        })
    }

    /// Record `object` among `environment`'s living members.
    pub(crate) fn mark_living(
        t: &mut Transaction,
        object: &Arc<Process>,
        environment: &Arc<Process>,
    ) {
        t.merge(
            environment.position.livings.id,
            MergeOp::ArrayAppend(vec![LpcRef::Object(Arc::downgrade(object))]),
        );
    }

    /// Drop `object` from `environment`'s living members.
    pub(crate) fn unmark_living(
        t: &mut Transaction,
        object: &Arc<Process>,
        environment: &Arc<Process>,
    ) {
        t.merge(
            environment.position.livings.id,
            MergeOp::ArrayRemoveValue(LpcRef::Object(Arc::downgrade(object))),
        );
    }

    /// Move `object` into `new_environment`, through the caller's transaction.
    ///
    /// The object's environment pointer is read (tracked) and written, so
    /// two moves of one object conflict and the loser re-runs; the room
    /// inventories change through merge writes, so moves of distinct
    /// objects through one room commute. A living mover also updates the
    /// rooms' living-member cells. Refuses a destination that is `object`
    /// or inside it, before any write.
    pub(crate) fn move_to(
        txn: &TxnHandle,
        object: &Arc<Process>,
        new_environment: &Arc<Process>,
    ) -> Result<(), WouldContainItself> {
        let object_cell = object.position.environment.id;
        let new_cell = new_environment.position.inventory.id;
        let new_env_ref = LpcRef::Object(Arc::downgrade(new_environment));
        let new_member = LpcRef::Object(Arc::downgrade(object));
        let mover_is_living = object.commands_enabled(txn);

        txn.with(|t| {
            Self::refuse_containment(t, object, new_environment)?;
            let current_env = Self::environment_of_inner(t, object);

            // Moving to the current environment is a no-op; without the check
            // the same reference would be added to the inventory twice.
            if current_env
                .as_ref()
                .is_some_and(|env| std::ptr::eq(env.as_ref(), new_environment.as_ref()))
            {
                return Ok(());
            }

            // Remove from the current environment's inventory, if it has one.
            if let Some(old) = current_env {
                t.merge(
                    old.position.inventory.id,
                    MergeOp::ArrayRemoveValue(new_member.clone()),
                );
                if mover_is_living {
                    Self::unmark_living(t, object, &old);
                }
            }

            // Add to the new environment's inventory and point the object at it.
            t.merge(new_cell, MergeOp::ArrayAppend(vec![new_member]));
            if mover_is_living {
                Self::mark_living(t, object, new_environment);
            }
            t.write(object_cell, new_env_ref);
            t.mark_presence_changed();
            Ok(())
        })
    }

    /// Whether `move_to(object, new_environment)` would be refused — for a
    /// caller with hooks to fire before the write.
    pub(crate) fn check_move(
        txn: &TxnHandle,
        object: &Arc<Process>,
        new_environment: &Arc<Process>,
    ) -> Result<(), WouldContainItself> {
        txn.with(|t| Self::refuse_containment(t, object, new_environment))
    }

    /// Refuse a destination that is `object` or inside it. The walk
    /// terminates because this refusal is what keeps the graph acyclic.
    fn refuse_containment(
        t: &mut Transaction,
        object: &Arc<Process>,
        new_environment: &Arc<Process>,
    ) -> Result<(), WouldContainItself> {
        let mut ancestor = Some(new_environment.clone());
        while let Some(env) = ancestor {
            if Arc::ptr_eq(&env, object) {
                return Err(WouldContainItself);
            }
            ancestor = Self::environment_of_inner(t, &env);
            debug_assert!(
                ancestor.is_none() || t.read(env.position.environment.id).is_some(),
                "environment traversal returned a parent without a stored link"
            );
        }
        Ok(())
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

    /// Whether this object is still live for `txn`: not removed by a
    /// committed destruct, nor by this attempt.
    pub(crate) fn is_live(&self, txn: &TxnHandle) -> bool {
        txn.with(|t| self.is_live_in(t))
    }

    pub(crate) fn is_live_in(&self, t: &mut Transaction) -> bool {
        let Some(&cell) = self.cell.get() else {
            return !self.physical.load(Ordering::Acquire) || t.bootstrap_object_is_live(self);
        };
        if t.is_removed(cell) {
            return false;
        }
        match t.read_object(cell) {
            Some(current) => std::ptr::eq(current.as_ref(), self),
            None => self.physical.load(Ordering::Acquire) && t.bootstrap_object_is_live(self),
        }
    }

    /// Whether the initializer has run, read through `txn` and tracked.
    pub(crate) fn is_initialized(&self, txn: &TxnHandle) -> bool {
        txn.with(|t| t.read(self.initialized.id).is_some())
    }

    /// [`is_live`](Self::is_live) and [`is_initialized`](Self::is_initialized)
    /// in one trip through the lock; a dead object's marker is not read.
    pub(crate) fn liveness(&self, txn: &TxnHandle) -> Liveness {
        txn.with(|t| {
            if !self.is_live_in(t) {
                Liveness::Dead
            } else if t.read(self.initialized.id).is_some() {
                Liveness::Ready
            } else {
                Liveness::Uninitialized
            }
        })
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
        matches!(self.name, ObjectName::Clone(_))
    }

    /// Whether `enable_commands()` is in effect, as seen through `txn`.
    pub(crate) fn commands_enabled(&self, txn: &TxnHandle) -> bool {
        txn.with(|t| t.read(self.commands_enabled.id).is_some())
    }

    /// A construction-time global cell for fixtures that have not upgraded.
    #[cfg(test)]
    pub(crate) fn var_id(&self, reg: lpc_rs_core::RegisterSize) -> VarId {
        self.initial_image.var_id(reg)
    }

    /// Resolve structural and global cell names only when emitting a diagnostic.
    pub(crate) fn describe_cell(&self, cell: VarId) -> Option<String> {
        if self
            .cleanup
            .as_ref()
            .is_some_and(|cleanup| cleanup.disabled.id == cell)
        {
            return Some(format!("{}.cleanup_disabled", self.filename()));
        }
        for (id, field) in [
            (self.initialized.id, "initialized"),
            (self.commands_enabled.id, "commands_enabled"),
            (self.rules.id, "rules"),
            (self.position.environment.id, "environment"),
            (self.position.inventory.id, "inventory"),
            (self.position.livings.id, "livings"),
            (self.connection.id, "connection"),
            (self.shadow.shadows.id, "shadows"),
            (self.shadow.shadowing.id, "shadowing"),
        ] {
            if cell == id {
                return Some(format!("{}.{field}", self.filename()));
            }
        }
        if cell == self.image_cell.id {
            return Some(format!("{}.program", self.filename()));
        }
        let program = self.initial_program();
        if cell == program.clones.id {
            return Some(format!("{}.clones", program.filename));
        }
        let index =
            (0..program.num_globals).find(|&slot| self.initial_image.var_id(slot) == cell)?;
        let name = self
            .initial_program()
            .global_variables
            .iter()
            .find_map(|(name, symbol)| match symbol.location {
                Some(lpc_rs_core::register::RegisterVariant::Global(register))
                    if register.index() == index =>
                {
                    Some(name.as_str())
                }
                _ => None,
            });
        Some(match name {
            Some(name) => format!("{}.global.{name}", self.filename()),
            None => format!("{}.global[{index}]", self.filename()),
        })
    }

    /// The world ids a live object keeps alive, rooted even when the object
    /// has no committed `Process` cell.
    pub(crate) fn world_var_ids(&self) -> Vec<VarId> {
        self.cleanup
            .as_ref()
            .map(|cleanup| cleanup.disabled.id)
            .into_iter()
            .chain([
                self.initialized.id,
                self.commands_enabled.id,
                self.rules.id,
                self.position.environment.id,
                self.position.inventory.id,
                self.position.livings.id,
                self.connection.id,
                self.shadow.shadows.id,
                self.shadow.shadowing.id,
                self.image_cell.id,
            ])
            .chain(
                self.snoop
                    .get()
                    .into_iter()
                    .flat_map(|s| [s.incoming, s.outgoing]),
            )
            .chain(self.socket_index.get().copied())
            .collect()
    }

    /// Get the filename of this process, including the clone ID suffix if
    /// present.
    #[inline]
    pub fn filename(&self) -> Cow<'_, str> {
        let filename: &str = self.initial_program().filename.as_ref().as_ref();
        let name = filename.strip_suffix(".c").unwrap_or(filename);
        match &self.name {
            ObjectName::File => Cow::Borrowed(name),
            ObjectName::Clone(id) => Cow::Owned(format!("{name}#{id}")),
            ObjectName::Virtual(path) => Cow::Borrowed(path),
        }
    }

    /// The object name for LPC output, with host-origin names localized safely.
    pub fn in_game_name(&self, root: LibRoot<'_>) -> String {
        match &self.name {
            ObjectName::Virtual(path) => {
                root.object_name(&LpcPath::in_game(path.into())).to_string()
            }
            ObjectName::File | ObjectName::Clone(_) => {
                let clone_id = match self.name {
                    ObjectName::Clone(id) => Some(id),
                    _ => None,
                };
                root.object_name(&self.initial_program().filename.object_file(clone_id))
                    .to_string()
            }
        }
    }

    /// The in-game directory of this object's name: what its relative object
    /// paths resolve against. A clone's is its program's; a virtual object's
    /// is the directory it was requested under.
    pub fn cwd(&self) -> PathBuf {
        in_game_dir(Path::new(&*self.filename()))
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
    }
}

impl Eq for Process {}

impl Hash for Process {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Nothing with interior mutability: `AllEnvironment` keeps processes
        // in a set.
        self.filename().as_ref().hash(state)
    }
}

impl Display for Process {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let source = self.initial_program().filename.to_string();
        let name = source.strip_suffix(".c").unwrap_or(&source);
        match &self.name {
            ObjectName::File => f.write_str(name),
            ObjectName::Clone(id) => write!(f, "{name}#{id}"),
            ObjectName::Virtual(path) => f.write_str(path),
        }
    }
}

/// `move_to` refused: the destination is the object or inside it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct WouldContainItself;

impl Display for WouldContainItself {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("would contain itself")
    }
}

impl std::error::Error for WouldContainItself {}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::interpreter::{
        object_space::ObjectSpace,
        stm::{Committer, txn_insert_process, txn_undo_insert},
    };

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

    fn program_at(filename: &str) -> Program {
        Program {
            filename: Arc::new(filename.into()),
            ..Default::default()
        }
    }

    #[test]
    fn object_names_keep_suffix_order_and_host_prefix_collisions() {
        let root = LibRoot::new("/d/root");
        let program: Arc<Program> = program_at("/d/root.c").into();
        assert_eq!(Process::new(program.clone()).in_game_name(root), "/");
        assert_eq!(
            Process::new_clone(program, 7).in_game_name(root),
            "/d/root#7"
        );

        let outside = Process::new(Program {
            filename: Arc::new(LpcPath::new_server("/private/elsewhere.c")),
            ..Default::default()
        });
        assert_eq!(outside.in_game_name(root), "<outside mudlib>");
        assert_eq!(outside.filename(), "/private/elsewhere");
        assert!(!outside.to_string().contains("private"));
    }

    #[test]
    fn a_virtual_object_is_named_as_requested() {
        let program: Arc<Program> = program_at("/d/room1.c").into();
        let proc = Process::new_virtual(program.clone(), "/inst/17/d/room1".to_owned());
        assert_eq!(proc.filename(), "/inst/17/d/room1");
        assert!(!proc.is_clone());
        assert!(Arc::ptr_eq(proc.initial_program(), &program));
    }

    #[test]
    fn a_clones_directory_is_its_programs() {
        let proc = Process::new_clone(program_at("/d/room1.c").into(), 7);
        assert_eq!(proc.filename(), "/d/room1#7");
        assert!(proc.is_clone());
        assert_eq!(proc.cwd().to_str().unwrap(), "/d");
    }

    #[test]
    fn a_virtual_objects_directory_is_where_it_was_requested() {
        let proc = Process::new_virtual(
            program_at("/d/room1.c").into(),
            "/inst/17/d/room1".to_owned(),
        );
        assert_eq!(proc.cwd().to_str().unwrap(), "/inst/17/d");
    }

    #[test]
    fn a_root_objects_directory_is_the_root() {
        assert_eq!(
            Process::new(program_at("/x.c")).cwd().to_str().unwrap(),
            "/"
        );
        assert_eq!(Process::new(program_at("x.c")).cwd().to_str().unwrap(), "/");
    }

    #[tokio::test]
    async fn an_object_starts_with_no_rules() {
        let vm = crate::interpreter::vm::Vm::new(crate::test_support::test_config());
        let proc = vm
            .initialize_process_from_code("/ob.c", "void create() {}")
            .await
            .unwrap()
            .context
            .process;
        use crate::interpreter::CommittedReader;
        assert!(vm.global_state.committed_rules(&proc).is_empty());
    }

    #[test]
    fn a_resident_process_is_uninitialized_until_the_claim() {
        let proc = Process::new(program_at("/liveness.c"));
        let txn = TxnHandle::empty();

        assert_eq!(proc.liveness(&txn), Liveness::Uninitialized);
    }

    #[test]
    fn a_claimed_init_makes_the_process_ready() {
        let proc = Process::new(program_at("/liveness.c"));
        let txn = TxnHandle::empty();
        assert!(proc.claim_init(&txn));

        assert_eq!(proc.liveness(&txn), Liveness::Ready);
    }

    #[test]
    fn a_destruct_in_the_attempt_makes_the_process_dead() {
        let proc = Process::new(program_at("/liveness.c"));
        let txn = TxnHandle::empty();
        assert!(proc.claim_init(&txn));
        let cell = *proc.cell.get_or_init(VarId::new);
        txn.with(|t| t.drop_var(cell));

        assert_eq!(proc.liveness(&txn), Liveness::Dead);
    }

    fn populated_inventory() -> (Committer, Arc<ObjectSpace>, Arc<Process>, Arc<Process>) {
        let mut committer = Committer::new();
        let space = Arc::new(ObjectSpace::default());
        let room = Arc::new(Process::new(program_at("/room.c")));
        let item = Arc::new(Process::new(program_at("/item.c")));
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_insert_process(&writer, &space, &room);
        txn_insert_process(&writer, &space, &item);
        writer.with(|t| t.write(item.commands_enabled.id, 1.into()));
        Process::move_to(&writer, &item, &room).unwrap();
        committer
            .commit(writer.with(|t| t.take_changeset()))
            .unwrap();
        (committer, space, room, item)
    }

    #[test]
    fn inventory_removal_respects_snapshots_and_invalidates_concurrent_readers() {
        let (mut committer, space, room, item) = populated_inventory();
        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert_eq!(
            Process::inventory_of(&reader, &room).as_slice(),
            std::slice::from_ref(&item)
        );

        let remover = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_undo_insert(&remover, &space, &item);
        assert!(Process::inventory_of(&remover, &room).is_empty());
        assert!(Process::livings_of(&remover, &room).is_empty());
        committer
            .commit(remover.with(|t| t.take_changeset()))
            .unwrap();

        assert_eq!(Process::inventory_of(&reader, &room), [item]);
        assert!(
            committer
                .commit(reader.with(|t| t.take_changeset()))
                .is_err()
        );
        let fresh = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert!(Process::inventory_of(&fresh, &room).is_empty());
        assert!(Process::livings_of(&fresh, &room).is_empty());
    }

    #[test]
    fn inventory_replacement_at_the_same_path_does_not_revive_old_membership() {
        let (mut committer, space, room, old) = populated_inventory();
        let new = Arc::new(Process::new(program_at("/item.c")));
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_undo_insert(&writer, &space, &old);
        txn_insert_process(&writer, &space, &new);
        assert!(Process::inventory_of(&writer, &room).is_empty());
        Process::move_to(&writer, &new, &room).unwrap();
        let inventory = Process::inventory_of(&writer, &room);
        assert_eq!(inventory.len(), 1);
        assert!(Arc::ptr_eq(&inventory[0], &new));
        committer
            .commit(writer.with(|t| t.take_changeset()))
            .unwrap();

        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let inventory = Process::inventory_of(&reader, &room);
        assert_eq!(inventory.len(), 1);
        assert!(Arc::ptr_eq(&inventory[0], &new));
        assert!(!old.is_live(&reader));
    }

    #[test]
    fn inventory_includes_pending_insertions_and_ignores_unrelated_writes() {
        let (mut committer, space, room, first) = populated_inventory();
        let second = Arc::new(Process::new(program_at("/second.c")));
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_insert_process(&writer, &space, &second);
        Process::move_to(&writer, &second, &room).unwrap();
        assert_eq!(
            Process::inventory_of(&writer, &room),
            [first.clone(), second.clone()]
        );
        committer
            .commit(writer.with(|t| t.take_changeset()))
            .unwrap();

        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert_eq!(Process::inventory_of(&reader, &room), [first, second]);
        let mut unrelated = Transaction::new(committer.snapshot_clone());
        unrelated.write(VarId::new(), 42.into());
        committer.commit(unrelated.take_changeset()).unwrap();
        committer
            .commit(reader.with(|t| t.take_changeset()))
            .unwrap();
    }

    #[test]
    fn living_inventory_is_visible_before_removal_and_stable_in_old_snapshots() {
        let (mut committer, space, room, item) = populated_inventory();
        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let living = Process::livings_of(&reader, &room);
        assert_eq!(living.len(), 1);
        assert!(Arc::ptr_eq(&living[0], &item));
        let remover = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_undo_insert(&remover, &space, &item);
        assert!(Process::livings_of(&remover, &room).is_empty());
        committer
            .commit(remover.with(Transaction::take_changeset))
            .unwrap();
        let retained = Process::livings_of(&reader, &room);
        assert_eq!(retained.len(), 1);
        assert!(Arc::ptr_eq(&retained[0], &item));
        let fresh = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert!(Process::livings_of(&fresh, &room).is_empty());
        assert!(
            committer
                .commit(reader.with(Transaction::take_changeset))
                .is_err()
        );
    }

    #[test]
    fn moving_a_living_object_updates_both_membership_cells() {
        let (mut committer, space, room, item) = populated_inventory();
        let before = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let destination = Arc::new(Process::new(program_at("/destination.c")));
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_insert_process(&writer, &space, &destination);
        Process::move_to(&writer, &item, &destination).unwrap();
        assert!(Process::livings_of(&writer, &room).is_empty());
        let moved = Process::livings_of(&writer, &destination);
        assert_eq!(moved.len(), 1);
        assert!(Arc::ptr_eq(&moved[0], &item));
        committer
            .commit(writer.with(Transaction::take_changeset))
            .unwrap();
        let retained = Process::livings_of(&before, &room);
        assert_eq!(retained.len(), 1);
        assert!(Arc::ptr_eq(&retained[0], &item));
        let fresh = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert!(Process::livings_of(&fresh, &room).is_empty());
        let moved = Process::livings_of(&fresh, &destination);
        assert_eq!(moved.len(), 1);
        assert!(Arc::ptr_eq(&moved[0], &item));
    }

    #[test]
    fn bootstrap_membership_and_pending_instances_have_distinct_liveness_rules() {
        use crate::interpreter::stm::{Snapshot, Version};

        let space = ObjectSpace::default();
        let bootstrap = Arc::new(Process::new(program_at("/bootstrap.c")));
        ObjectSpace::insert_process_physical(&space, bootstrap.clone());
        let mut snapshot = Snapshot::new(Version::new(), imbl::HashMap::new());
        snapshot
            .objects
            .insert("/bootstrap".into(), bootstrap.clone());
        let before = TxnHandle::new(Transaction::new(snapshot.clone()));
        assert!(bootstrap.is_live(&before));
        let pending = Process::new(program_at("/bootstrap.c"));
        assert!(pending.is_live(&before));
        bootstrap.cell.get_or_init(VarId::new);
        snapshot.objects.remove("/bootstrap");
        let after = TxnHandle::new(Transaction::new(snapshot));
        assert!(!bootstrap.is_live(&after));
        assert_eq!(bootstrap.liveness(&after), Liveness::Dead);
        assert!(bootstrap.is_live(&before));
    }

    #[tokio::test]
    async fn diagnostic_cell_names_cover_structural_cleanup_program_and_named_globals() {
        let (program, _config, _simul) = crate::test_support::compile_prog(
            "int alpha; int beta; int clean_up(int count) { return 0; }",
        )
        .await;
        let process = Process::new(program);
        let name = process.filename().into_owned();
        for (cell, field) in [
            (process.initialized.id, "initialized"),
            (process.commands_enabled.id, "commands_enabled"),
            (process.rules.id, "rules"),
            (process.position.environment.id, "environment"),
            (process.position.inventory.id, "inventory"),
            (process.position.livings.id, "livings"),
            (process.connection.id, "connection"),
            (process.shadow.shadows.id, "shadows"),
            (process.shadow.shadowing.id, "shadowing"),
            (process.image_cell.id, "program"),
            (
                process.cleanup.as_ref().unwrap().disabled.id,
                "cleanup_disabled",
            ),
        ] {
            assert_eq!(process.describe_cell(cell), Some(format!("{name}.{field}")));
        }
        assert_eq!(
            process.describe_cell(process.initial_program().clones.id),
            Some(format!("{}.clones", process.initial_program().filename)),
        );
        for global in ["alpha", "beta"] {
            let symbol = &process.initial_program().global_variables[global];
            let Some(lpc_rs_core::register::RegisterVariant::Global(register)) = symbol.location
            else {
                panic!("global declaration needs a global register");
            };
            assert_eq!(
                process.describe_cell(process.var_id(register.index())),
                Some(format!("{name}.global.{global}")),
            );
        }
        assert_eq!(process.describe_cell(VarId::new()), None);
    }

    #[test]
    fn diagnostic_global_names_fall_back_to_the_slot_without_source_symbols() {
        let process = Process::new(Program {
            filename: Arc::new("/anonymous.c".into()),
            num_globals: 1,
            ..Program::default()
        });
        assert_eq!(
            process.describe_cell(process.var_id(0)),
            Some("/anonymous.global[0]".into())
        );
        assert_eq!(process.describe_cell(VarId::new()), None);
    }

    #[test]
    fn process_equality_and_hashing_follow_the_stable_object_name() {
        use std::collections::hash_map::DefaultHasher;

        let file = Process::new(program_at("/identity.c"));
        let same = Process::new(program_at("/identity.c"));
        let different = Process::new(program_at("/different.c"));
        assert_eq!(file, same);
        assert_ne!(file, different);
        let clone = Process::new_clone(program_at("/identity.c").into(), 1);
        let other_clone = Process::new_clone(program_at("/identity.c").into(), 2);
        assert_ne!(file, clone);
        assert_ne!(clone, other_clone);
        let virtual_one = Process::new_virtual(program_at("/one.c").into(), "/virtual".into());
        let virtual_two = Process::new_virtual(program_at("/two.c").into(), "/virtual".into());
        assert_eq!(virtual_one, virtual_two);
        let hash = |process: &Process| {
            let mut hasher = DefaultHasher::new();
            process.hash(&mut hasher);
            hasher.finish()
        };
        assert_eq!(hash(&file), hash(&same));
        assert_eq!(hash(&virtual_one), hash(&virtual_two));
        let before = hash(&file);
        let txn = TxnHandle::empty();
        assert!(file.claim_init(&txn));
        txn.with(|t| t.write(file.commands_enabled.id, 1.into()));
        assert_eq!(hash(&file), before);
        assert_eq!(file, same);
        let distinct = [&file, &different, &clone, &other_clone, &virtual_one]
            .into_iter()
            .map(hash)
            .collect::<std::collections::HashSet<_>>();
        assert!(
            distinct.len() > 1,
            "distinct object names collapsed to one hash"
        );
    }
}
