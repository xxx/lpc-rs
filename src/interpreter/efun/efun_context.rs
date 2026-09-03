use std::{fmt::Debug, path::PathBuf, sync::Arc};

use arc_swap::ArcSwapAny;
use delegate::delegate;
use lpc_rs_core::{RegisterSize, lpc_path::LpcPath};
use lpc_rs_errors::{LpcError, Result, span::Span};
use lpc_rs_utils::config::Config;

use crate::command::{presence::forget_destruct, registry::VerbRules};
use crate::interpreter::{
    call_frame::CallFrame,
    call_stack::CallStack,
    lpc_array::LpcArray,
    lpc_ref::LpcRef,
    object_space::ObjectSpace,
    process::Process,
    stm::{Effect, TxnHandle},
    task_context::{Caller, Loader, ObjectLookup, TaskContext},
};

/// A structure to hold various pieces of interpreter state, to be passed to
/// Efuns when they're called
#[derive(Debug)]
pub struct EfunContext<'task, const N: usize> {
    stack: &'task mut CallStack<N>,
    task_context: &'task TaskContext,

    /// Allow the user to take a snapshot of the callstack, for testing and
    /// debugging
    #[cfg(test)]
    pub snapshot: Option<CallStack<N>>,
}

impl<'task, const N: usize> EfunContext<'task, N> {
    pub fn new(stack: &'task mut CallStack<N>, task_context: &'task TaskContext) -> Self {
        Self {
            stack,
            task_context,

            #[cfg(test)]
            snapshot: None,
        }
    }

    delegate! {
        to self.task_context {
            /// Get pointer to the current [`Config`] that's in-use
            pub fn config(&self) -> &Arc<Config>;

            /// Schedule a call out transactionally.
            pub fn schedule_call_out(
                &self,
                owner: &Arc<Process>,
                func_ref: LpcRef,
                delay: chrono::Duration,
                repeat: Option<chrono::Duration>,
            ) -> u64;

            /// Query a call out transactionally.
            pub fn query_call_out(&self, id: u64) -> Option<Vec<LpcRef>>;

            /// Query all call outs of `owner` transactionally.
            pub fn query_call_outs(&self, owner: &Arc<Process>) -> Vec<Vec<LpcRef>>;

            /// Cancel a call out transactionally.
            pub fn cancel_call_out(&self, id: u64) -> i64;
        }
    }

    /// Find or load the object `arg` names, resolved against the caller's
    /// directory. Loading is transactional: the master's `valid_load` is
    /// asked, a new object gets a cell write and deferred physical insert,
    /// and its initializer runs in a sub-task that joins this transaction; a
    /// throwing initializer undoes the insert instead of leaving it resident.
    pub async fn load_object(&self, arg: &str) -> Result<Arc<Process>> {
        let func = self.frame().function.name().to_string();
        let path = self
            .task_context
            .object_path(arg, self.in_game_cwd(), &func)
            .map_err(|e| e.with_span(self.call_site_span()))?;
        if let ObjectLookup::Found(proc) = self.find_object(&path) {
            return Ok(proc);
        }

        let loader = Loader {
            func,
            chain: self.chain(),
            program: self.calling_program(),
        };
        let process = self
            .task_context
            .compile_process(&path, &loader)
            .await
            .map_err(|e| self.loaded_from_here(e))?;

        self.task_context
            .insert_and_initialize(loader.callers(), &process)
            .await
            .map_err(|e| self.loaded_from_here(e))?;

        Ok(process)
    }

    /// An error from loading an object keeps its own location; the call
    /// site becomes a label, or the location when it had none.
    fn loaded_from_here(&self, e: LpcError) -> LpcError {
        let call_site = self.call_site_span();
        if e.span().is_some() {
            e.with_label("loaded from here", call_site)
        } else {
            e.with_span(call_site)
        }
    }

    /// The span of the instruction that called this efun; an efun's own
    /// frame carries no spans.
    #[inline]
    pub fn call_site_span(&self) -> Option<Span> {
        self.previous_debug_span()
            .or_else(|| self.current_debug_span())
    }

    /// Get a reference to the current [`CallFrame`]
    #[inline]
    pub fn frame(&self) -> &CallFrame {
        self.stack.last().unwrap()
    }

    /// The in-game directory of the object executing this efun — the frame's,
    /// not the task entry's, so a room reached by `->` resolves its own exits.
    pub fn in_game_cwd(&self) -> PathBuf {
        self.task_context.in_game_cwd_of(&self.frame().process)
    }

    /// Place `result` in the frame's return register. An int into an int
    /// register is a scalar store: the 16-byte enum move stalls on its own
    /// spill.
    #[inline(always)]
    pub fn return_efun_result(&mut self, result: LpcRef) {
        let slot = &mut self.stack.last_mut().unwrap().registers[0];
        match result {
            LpcRef::Int(value) => match slot {
                LpcRef::Int(x) => x.0 = value.0,
                _ => *slot = LpcRef::Int(value),
            },
            other => *slot = other,
        }
    }

    /// Write `value` back through by-reference argument `index` (0-based).
    pub(crate) fn write_ref(&self, index: RegisterSize, value: LpcRef) -> Result<()> {
        let Some(&(_, cell)) = self.frame().ref_cells.iter().find(|(i, _)| *i == index) else {
            return Err(self.runtime_bug(format!(
                "argument {index} of `{}` is not a by-reference argument",
                self.frame().function.name()
            )));
        };
        self.txn().with(|t| t.write(cell, value));
        Ok(())
    }

    /// Mint `items` as an array in this efun's transaction.
    pub(crate) fn mint_array<I>(&self, items: I) -> LpcRef
    where
        I: IntoIterator<Item = LpcRef>,
    {
        let array = items.into_iter().collect::<LpcArray>();
        LpcRef::Array(self.txn().with(|t| t.mint_array(array)))
    }

    /// Mint `items` as an array and return it from the efun.
    pub(crate) fn return_array<I>(&mut self, items: I)
    where
        I: IntoIterator<Item = LpcRef>,
    {
        let result = self.mint_array(items);
        self.return_efun_result(result);
    }

    /// Resolve `path` against the current process's in-game directory.
    pub fn in_game_path(&self, path: &str) -> LpcPath {
        LpcPath::new_in_game(path, self.in_game_cwd(), &*self.config().lib_dir)
    }

    /// Read a call-out id argument: a non-int is a bug, a negative id an error.
    pub fn call_out_id<I>(&self, register: I, efun_name: &str) -> Result<u64>
    where
        I: Into<RegisterSize>,
    {
        let LpcRef::Int(idx) = self.resolve_local_register(register) else {
            return Err(self.runtime_bug(format!("non-int call out ID sent to `{efun_name}`")));
        };
        if idx.0 < 0 {
            return Err(
                self.runtime_error(format!("invalid call out ID `{idx}` sent to `{efun_name}`"))
            );
        }
        Ok(idx.0 as u64)
    }

    /// Get the current debug span
    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        self.frame().current_debug_span()
    }

    /// Get the current debug span of the previous frame
    #[inline]
    pub fn previous_debug_span(&self) -> Option<Span> {
        if self.stack.len() > 1 {
            self.stack[self.stack.len() - 2].current_debug_span()
        } else {
            None
        }
    }

    /// A runtime error located at the instruction that called this efun.
    #[inline]
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        LpcError::runtime(msg).with_span(self.call_site_span())
    }

    /// A runtime bug located at the instruction that called this efun.
    #[inline]
    pub fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> LpcError {
        LpcError::runtime_bug(msg).with_span(self.call_site_span())
    }

    /// Resolve a local register
    #[inline]
    pub fn resolve_local_register<I>(&self, register: I) -> &LpcRef
    where
        I: Into<RegisterSize>,
    {
        &self.frame().registers[register.into()]
    }

    /// Resolve a local register
    #[inline]
    pub fn try_resolve_local_register<I>(&self, register: I) -> Option<&LpcRef>
    where
        I: Into<usize>,
    {
        self.frame().registers.get(register.into())
    }

    /// The transaction this efun's task runs in.
    #[inline]
    pub(crate) fn txn(&self) -> &TxnHandle {
        self.task_context.txn()
    }

    /// The object space this efun's task commits into.
    #[inline]
    pub(crate) fn object_space(&self) -> &ObjectSpace {
        self.task_context.object_space()
    }

    /// Record a physical side effect on this efun's attempt.
    pub(crate) fn record_effect(&self, effect: Effect) {
        self.txn().with(|t| t.record_effect(effect));
    }

    /// Find an object by path, transactionally. Delegates to
    /// [`TaskContext::find_object`]; does not initialize or create.
    pub fn find_object(&self, path: &LpcPath) -> ObjectLookup {
        self.task_context.find_object(path)
    }

    /// Remove the passed [`Process`] from the object space, transactionally:
    /// drop its cell (so it reads back as absent to this attempt, and a
    /// concurrent reader of the cell conflicts and re-runs) and record a
    /// deferred physical removal applied at commit.
    #[inline]
    pub fn remove_process<P>(&self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        let process = process.into();
        let key = self.object_space().process_key(&process);
        let var_id = *process
            .cell
            .get_or_init(|| self.object_space().cell_id(&key));
        let is_living = process.commands_enabled(self.txn());
        let environment = Process::environment_of(self.txn(), &process);
        forget_destruct(self.txn(), &process);
        let connection = self
            .txn()
            .with(|t| t.read_connection(process.connection.id));
        if let Some(connection) = connection {
            self.txn()
                .with(|t| t.write_connection(process.connection.id, None));
            self.record_effect(Effect::Disconnect {
                connection,
                message: None,
            });
        }
        // A destructed verb object's parser rules go with it; without the
        // `parse_init()` gate every destruct would write `verb_rules`.
        if process.parser_ready.get().is_some() {
            VerbRules::new(self.task_context()).remove_owner(&process);
        }
        self.txn().with(|t| {
            t.drop_var(var_id);
            t.drop_var(process.rules.id);
            t.drop_var(process.position.livings.id);
            if is_living && let Some(env) = &environment {
                Process::unmark_living(t, &process, env);
            }
        });
        self.record_effect(Effect::RemoveObject { key, process });
    }

    /// The task context this efun runs in.
    #[inline]
    pub fn task_context(&self) -> &TaskContext {
        self.task_context
    }

    /// The objects that called through a door to reach the code running this
    /// efun, innermost first: the stack's crossers, then the task's chain.
    pub fn previous_objects(&self) -> impl Iterator<Item = &Arc<Process>> {
        self.stack
            .door_crossers(self.stack.len() - 1)
            .chain(Caller::objects(&self.task_context.callers))
    }

    /// The chain a task this efun starts is entered with: the object running
    /// it, and what `previous_object` answers there.
    pub fn chain(&self) -> Arc<Caller> {
        self.stack
            .chain(self.stack.len() - 1, self.task_context.callers.clone())
    }

    /// Get a reference to the [`Process`] that contains the call to this efun
    #[inline]
    pub fn process(&self) -> &Arc<Process> {
        &self.frame().process
    }

    /// The defining file of the code that called this efun — the file that
    /// wrote the pointer when it was fired through one, else the nearest
    /// frame under it that is not an efun's — as an in-game path with its
    /// extension (`/secure/master.c`); `NULL` when there is none (an efun
    /// pointer fired as a task's entry).
    pub(crate) fn calling_program(&self) -> LpcRef {
        self.stack.calling_program(self.config().lib_dir.as_str())
    }

    /// Get a reference to `this_player` from the context
    #[inline]
    pub fn this_player(&self) -> &ArcSwapAny<Option<Arc<Process>>> {
        &self.task_context.this_player
    }

    /// Get the current `chain_count` from the context.
    #[inline]
    pub fn chain_count(&self) -> u8 {
        self.task_context.chain_count
    }

    /// Return a clone of the current stack, for snapshotting
    #[cfg(test)]
    pub fn clone_stack(&self) -> CallStack<N> {
        self.stack.clone()
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_type::LpcType;
    use lpc_rs_function_support::{
        function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunctionBuilder,
    };
    use lpc_rs_utils::lpc_string::LpcString;

    use super::*;
    use crate::{
        interpreter::{
            program::ProgramBuilder,
            vm::{global_state::GlobalState, vm_op::VmOp},
        },
        test_support::test_config,
    };

    /// A fresh, uncommitted `EfunContext` whose call stack holds one real
    /// frame: `load_object` records the caller's debug span, so `frame()`
    /// must resolve.
    fn efun_context() -> (TaskContext, CallStack<10>) {
        let (tx, _rx) = tokio::sync::mpsc::channel::<VmOp>(128);
        let global_state = GlobalState::new(test_config(), tx);
        let program = ProgramBuilder::default()
            .filename(LpcPath::InGame("/caller".into()))
            .build()
            .expect("program builder");
        let process = Arc::new(Process::new(program));
        let task_context = TaskContext::new(Arc::new(global_state), process.clone(), None);

        let function = ProgramFunctionBuilder::default()
            .prototype(
                FunctionPrototypeBuilder::default()
                    .name("efun_test")
                    .filename(Arc::new(LpcPath::InGame("/caller".into())))
                    .return_type(LpcType::Void)
                    .build()
                    .expect("prototype builder"),
            )
            .build()
            .expect("function builder");
        let frame = CallFrame::new(
            process,
            Arc::new(function),
            0 as RegisterSize,
            None::<&[crate::interpreter::stm::VarId]>,
        );
        let mut stack = CallStack::default();
        stack.push(frame).expect("push entry frame");

        (task_context, stack)
    }

    // `destruct` + re-create of a prototype, repeated in one transaction. The
    // removal is deferred to commit, so until then the physical map and the
    // committed world both still hold the destructed process; every
    // re-create must yield a distinct process identity (no resurrection from
    // either source), and a find with no intervening destruct returns the
    // same identity again. Object identity is only observable in Rust
    // (LPC `==` compares the weak handles, not the processes; a prototype's
    // `ObjectName` carries nothing to compare), so this asserts `Arc`
    // identity.
    #[tokio::test]
    async fn destruct_and_recreate_cycles_yield_fresh_objects() {
        let (task_context, mut stack) = efun_context();
        crate::test_support::permissive_master(&task_context.global_state.object_space).await;
        let ctx = EfunContext::new(&mut stack, &task_context);

        let path = LpcPath::new_in_game(
            "/example",
            task_context.in_game_cwd_of(&task_context.process),
            &*test_config().lib_dir,
        );

        // Baseline: create once, and a second find (no destruct in between)
        // returns the same object.
        let p1 = ctx.load_object("/example").await.expect("first create");
        let ObjectLookup::Found(p1b) = ctx.find_object(&path) else {
            panic!("second find, no destruct, should be Found");
        };
        assert!(
            Arc::ptr_eq(&p1, &p1b),
            "find without a destruct returns the same object"
        );

        // Cycle 1: destruct + re-create. The re-created object must be a
        // distinct identity, even though the physical map and committed world
        // still hold `p1` (the effects haven't landed).
        ctx.remove_process(p1.clone());
        let p2 = ctx
            .load_object("/example")
            .await
            .expect("re-create after destruct");
        assert!(
            !Arc::ptr_eq(&p1, &p2),
            "re-created object must not resurrect the destructed one"
        );

        // Cycle 2: the same sequence again; all three identities distinct.
        ctx.remove_process(p2.clone());
        let p3 = ctx.load_object("/example").await.expect("second re-create");
        assert!(
            !Arc::ptr_eq(&p2, &p3) && !Arc::ptr_eq(&p1, &p3),
            "second re-create must be a fresh object, distinct from both predecessors"
        );

        // And a plain find (no destruct) after the cycles returns the live
        // (most recent) object's identity.
        let ObjectLookup::Found(p3b) = ctx.find_object(&path) else {
            panic!("find after the cycles, should be Found");
        };
        assert!(
            Arc::ptr_eq(&p3, &p3b),
            "find returns the live object's identity"
        );
    }

    /// A never-suspending efun answers the sync dispatcher; one that
    /// awaits is left to the async one.
    #[test]
    fn the_sync_dispatcher_runs_only_efuns_that_never_suspend() {
        use crate::interpreter::efun::{Efun, call_efun_sync};

        let (task_context, mut stack) = efun_context();
        let mut ctx = EfunContext::new(&mut stack, &task_context);
        assert!(call_efun_sync(Efun::this_object, &mut ctx).is_some());
        assert!(call_efun_sync(Efun::clone_object, &mut ctx).is_none());
    }

    #[test]
    fn an_int_result_lands_in_register_zero() {
        let (task_context, mut stack) = efun_context();
        let mut ctx = EfunContext::new(&mut stack, &task_context);
        ctx.return_efun_result(LpcRef::from(7));
        assert_eq!(ctx.frame().registers[0], LpcRef::from(7));
    }

    #[test]
    fn an_int_result_replaces_a_register_holding_another_type() {
        let (task_context, mut stack) = efun_context();
        stack.last_mut().unwrap().registers[0] = LpcString::from("was a string").into();
        let mut ctx = EfunContext::new(&mut stack, &task_context);
        ctx.return_efun_result(LpcRef::from(true));
        assert_eq!(ctx.frame().registers[0], LpcRef::from(1));
    }

    #[test]
    fn a_non_int_result_lands_in_register_zero() {
        let (task_context, mut stack) = efun_context();
        let mut ctx = EfunContext::new(&mut stack, &task_context);
        ctx.return_efun_result(LpcString::from("result").into());
        assert_eq!(
            ctx.frame().registers[0],
            LpcRef::from(LpcString::from("result"))
        );
    }

    #[test]
    fn write_ref_to_an_index_with_no_cell_is_a_runtime_bug() {
        let (task_context, mut stack) = efun_context();
        let ctx = EfunContext::new(&mut stack, &task_context);
        let err = ctx.write_ref(2, LpcRef::from(1)).unwrap_err().to_string();
        assert!(
            err.contains("argument 2 of `efun_test` is not a by-reference argument"),
            "{err}"
        );
    }

    /// The frame under the efun's is the caller; its file is the program.
    #[test]
    fn calling_program_is_the_file_of_the_frame_under_the_efun() {
        use crate::interpreter::{efun::EFUN_FUNCTIONS, stm::VarId};

        let (task_context, mut stack) = efun_context();
        let process = stack.last().unwrap().process.clone();
        let efun_frame = CallFrame::new(
            process,
            EFUN_FUNCTIONS["this_object"].clone(),
            0 as RegisterSize,
            None::<&[VarId]>,
        );
        stack.push(efun_frame).unwrap();
        let ctx = EfunContext::new(&mut stack, &task_context);
        assert_eq!(ctx.calling_program().as_str(), Some("/caller"));
    }

    /// An efun frame fired through a pointer reports the pointer's writer,
    /// not the frame under it.
    #[test]
    fn an_efun_frame_fired_through_a_pointer_reports_the_pointers_origin() {
        use crate::interpreter::{efun::EFUN_FUNCTIONS, stm::VarId};

        let (task_context, mut stack) = efun_context();
        let process = stack.last().unwrap().process.clone();
        let mut efun_frame = CallFrame::new(
            process,
            EFUN_FUNCTIONS["this_object"].clone(),
            0 as RegisterSize,
            None::<&[VarId]>,
        );
        efun_frame.origin = Some(Arc::new(LpcPath::InGame("/writer.c".into())));
        stack.push(efun_frame).unwrap();
        let ctx = EfunContext::new(&mut stack, &task_context);
        assert_eq!(ctx.calling_program().as_str(), Some("/writer.c"));
    }

    /// An efun fired as a task's entry has no LPC caller.
    #[test]
    fn calling_program_is_zero_without_an_lpc_frame() {
        use crate::interpreter::{efun::EFUN_FUNCTIONS, stm::VarId};

        let (task_context, mut stack) = efun_context();
        let process = stack.pop().unwrap().process;
        let efun_frame = CallFrame::new(
            process,
            EFUN_FUNCTIONS["this_object"].clone(),
            0 as RegisterSize,
            None::<&[VarId]>,
        );
        stack.push(efun_frame).unwrap();
        let ctx = EfunContext::new(&mut stack, &task_context);
        assert!(ctx.calling_program().is_null());
    }
}
