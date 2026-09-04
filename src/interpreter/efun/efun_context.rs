use std::{fmt::Debug, path::PathBuf, sync::Arc};

use arc_swap::ArcSwapAny;
use delegate::delegate;
use lpc_rs_asm::instruction::{Arg, ArgList};
use lpc_rs_core::{RegisterSize, lpc_path::LpcPath};
use lpc_rs_errors::{LpcError, Result, span::Span};
use lpc_rs_utils::config::Config;
use smallvec::SmallVec;

use crate::command::{presence::forget_destruct, registry::VerbRules};
use crate::interpreter::{
    call_frame::CallFrame,
    call_stack::CallStack,
    efun::Efun,
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::{LpcRef, NULL},
    object_space::ObjectSpace,
    process::Process,
    stm::{Effect, TxnHandle},
    task_context::{Caller, Loader, ObjectLookup, TaskContext},
};

/// An efun fired through a pointer: the object it runs as, and the
/// pointer's writer.
#[derive(Debug)]
struct Fired {
    owner: Arc<Process>,
    origin: Option<Arc<LpcPath>>,
}

const NO_CALLING_FRAME: &str = "an efun at a call site has its calling frame";

/// An efun's view of its call: its arguments, the object it runs as and
/// the calling frame, with no frame of its own.
#[derive(Debug)]
pub struct EfunContext<'task, const N: usize> {
    stack: &'task mut CallStack<N>,
    task_context: &'task TaskContext,

    /// The efun being called.
    efun: Efun,

    /// The argument values in call order, padded with `NULL` to the
    /// prototype's arity; a by-reference argument is its cell's value at
    /// the call.
    args: SmallVec<[LpcRef; 4]>,

    /// How many arguments the call passed.
    passed: usize,

    /// The calling instruction's list, where a by-reference cell is found;
    /// `None` for a fired efun.
    list: Option<ArgList>,

    /// `Some` when fired through a pointer; else the efun is the calling
    /// frame's.
    fired: Option<Fired>,

    /// The result of an efun run with no frame under it.
    entry_result: Option<LpcRef>,

    /// Allow the user to take a snapshot of the callstack, for testing and
    /// debugging
    #[cfg(test)]
    pub snapshot: Option<CallStack<N>>,
}

impl<'task, const N: usize> EfunContext<'task, N> {
    /// A context for `efun` with no arguments, at a call site.
    pub fn new(
        stack: &'task mut CallStack<N>,
        task_context: &'task TaskContext,
        efun: Efun,
    ) -> Self {
        Self::build(stack, task_context, efun, SmallVec::new(), None, None)
    }

    /// The context for `efun`, called by the top frame's instruction with
    /// `list`: the values read now, a by-reference cell's with them.
    pub(crate) fn at_call(
        stack: &'task mut CallStack<N>,
        task_context: &'task TaskContext,
        efun: Efun,
        list: ArgList,
    ) -> Result<Self> {
        let caller = stack.current_frame()?;
        let txn = task_context.txn();
        let prototype = efun.prototype();
        let mut args = SmallVec::new();
        for (i, arg) in caller.function.args(list).iter().enumerate() {
            let value = match *arg {
                Arg::Value(location) => {
                    if prototype.is_ref_param(i) {
                        return Err(caller.runtime_error(format!(
                            "argument {} of `{}` must be passed by reference",
                            i + 1,
                            prototype.name
                        )));
                    }
                    caller.get_location(txn, location)?.into_owned()
                }
                Arg::Ref(location) => {
                    if !prototype.is_ref_param(i) {
                        return Err(caller.runtime_error(format!(
                            "`{}` does not take argument {} by reference",
                            prototype.name,
                            i + 1
                        )));
                    }
                    let cell = caller.ref_cell(location)?;
                    txn.with(|t| t.read(cell).unwrap_or(NULL))
                }
            };
            args.push(value);
        }
        Ok(Self::build(
            stack,
            task_context,
            efun,
            args,
            Some(list),
            None,
        ))
    }

    /// The context for `efun` fired through a pointer: `args` already
    /// resolved, run as `owner`, written by `origin`; the stack may be
    /// empty.
    pub(crate) fn fired(
        stack: &'task mut CallStack<N>,
        task_context: &'task TaskContext,
        efun: Efun,
        args: Vec<LpcRef>,
        owner: Arc<Process>,
        origin: Option<Arc<LpcPath>>,
    ) -> Self {
        let fired = Fired { owner, origin };
        Self::build(
            stack,
            task_context,
            efun,
            SmallVec::from_vec(args),
            None,
            Some(fired),
        )
    }

    fn build(
        stack: &'task mut CallStack<N>,
        task_context: &'task TaskContext,
        efun: Efun,
        args: SmallVec<[LpcRef; 4]>,
        list: Option<ArgList>,
        fired: Option<Fired>,
    ) -> Self {
        let passed = args.len();
        let mut args = args;
        let declared = usize::from(efun.prototype().arity.num_args);
        if args.len() < declared {
            args.resize(declared, NULL);
        }
        // An efun that answers 0 by leaving the result slot alone relies on this.
        if let Some(frame) = stack.last_mut() {
            frame.registers[0] = NULL;
        }
        Self {
            stack,
            task_context,
            efun,
            args,
            passed,
            list,
            fired,
            entry_result: None,

            #[cfg(test)]
            snapshot: None,
        }
    }

    /// Close the call: an error gets the call-site span; `Some` is the
    /// result of an efun run with no frame under it, for the task.
    pub(crate) fn finish(self, result: Result<()>) -> Result<Option<LpcRef>> {
        result.map_err(|e| e.or_span(self.call_site_span()))?;
        Ok(self.entry_result)
    }

    /// The frame whose instruction called this efun.
    fn calling_frame(&self) -> &CallFrame {
        self.stack.last().expect(NO_CALLING_FRAME)
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
        let func = self.efun.name().to_string();
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

    /// The span of the instruction that called this efun; `None` for one
    /// fired as a task's entry.
    #[inline]
    pub fn call_site_span(&self) -> Option<Span> {
        self.stack.last().and_then(CallFrame::current_debug_span)
    }

    /// The in-game directory of the object running this efun — its own,
    /// not the task entry's, so a room reached by `->` resolves its own exits.
    pub fn in_game_cwd(&self) -> PathBuf {
        self.task_context.in_game_cwd_of(self.process())
    }

    /// Place `result` where the caller reads it: its register 0, or the
    /// task's result when no frame is under the efun. An int into an int
    /// register is a scalar store: the 16-byte enum move stalls on its own
    /// spill.
    #[inline(always)]
    pub fn return_efun_result(&mut self, result: LpcRef) {
        let Some(frame) = self.stack.last_mut() else {
            self.entry_result = Some(result);
            return;
        };
        let slot = &mut frame.registers[0];
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
        let cell = self.list.and_then(|list| {
            let caller = self.stack.last()?;
            match caller.function.args(list).get(usize::from(index)) {
                Some(&Arg::Ref(location)) => Some(caller.ref_cell(location)),
                _ => None,
            }
        });
        let Some(cell) = cell else {
            return Err(self.runtime_bug(format!(
                "argument {index} of `{}` is not a by-reference argument",
                self.efun.name()
            )));
        };
        let cell = cell?;
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

    /// Mint `mapping` in this efun's transaction and return it from the efun.
    pub(crate) fn return_mapping(&mut self, mapping: LpcMapping) {
        let result = LpcRef::Mapping(self.txn().with(|t| t.mint_mapping(mapping)));
        self.return_efun_result(result);
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
    pub fn call_out_id(&self, i: usize, efun_name: &str) -> Result<u64> {
        let LpcRef::Int(idx) = self.arg(i) else {
            return Err(self.runtime_bug(format!("non-int call out ID sent to `{efun_name}`")));
        };
        if idx.0 < 0 {
            return Err(
                self.runtime_error(format!("invalid call out ID `{idx}` sent to `{efun_name}`"))
            );
        }
        Ok(idx.0 as u64)
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

    /// Argument `i` (0-based); one past the arity is a bug.
    #[inline]
    pub fn arg(&self, i: usize) -> &LpcRef {
        &self.args[i]
    }

    /// Argument `i` (0-based); `None` past the arguments.
    #[inline]
    pub fn try_arg(&self, i: usize) -> Option<&LpcRef> {
        self.args.get(i)
    }

    /// How many arguments the call passed.
    #[inline]
    pub fn arg_count(&self) -> usize {
        self.passed
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
    /// efun, innermost first: the firer of a fired efun, the stack's
    /// crossers, then the task's chain.
    pub fn previous_objects(&self) -> impl Iterator<Item = &Arc<Process>> {
        let firer = self
            .fired
            .as_ref()
            .and_then(|_| self.stack.last())
            .map(|frame| &frame.process);
        let crossers = self
            .stack
            .len()
            .checked_sub(1)
            .into_iter()
            .flat_map(|top| self.stack.door_crossers(top));
        firer
            .into_iter()
            .chain(crossers)
            .chain(Caller::objects(&self.task_context.callers))
    }

    /// The chain a task this efun starts is entered with: the object running
    /// it, and what `previous_object` answers there.
    pub fn chain(&self) -> Arc<Caller> {
        let tail = self.task_context.callers.clone();
        let frames = self
            .stack
            .len()
            .checked_sub(1)
            .map(|top| self.stack.chain(top, tail.clone()));
        match &self.fired {
            Some(fired) => Caller::link(fired.owner.clone(), frames.or(tail)),
            None => frames.expect(NO_CALLING_FRAME),
        }
    }

    /// The object this efun runs as: the pointer's owner when fired
    /// through one, else the calling frame's.
    #[inline]
    pub fn process(&self) -> &Arc<Process> {
        match &self.fired {
            Some(fired) => &fired.owner,
            None => &self.calling_frame().process,
        }
    }

    /// The defining file of the code that called this efun — the file that
    /// wrote the pointer when it was fired through one, else the calling
    /// frame's — as an in-game path with its extension
    /// (`/secure/master.c`); `NULL` when there is neither (an efun pointer
    /// fired as a task's entry).
    pub(crate) fn calling_program(&self) -> LpcRef {
        let lib_dir = self.config().lib_dir.as_str();
        match self
            .fired
            .as_ref()
            .and_then(|fired| fired.origin.as_deref())
        {
            Some(origin) => LpcRef::from(origin.as_in_game(lib_dir).display().to_string()),
            None => self.stack.calling_program(lib_dir),
        }
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
        function_prototype::FunctionPrototypeBuilder,
        program_function::{ProgramFunction, ProgramFunctionBuilder},
    };
    use lpc_rs_utils::lpc_string::LpcString;

    use super::*;
    use crate::{
        interpreter::{
            efun::Efun,
            program::ProgramBuilder,
            vm::{global_state::GlobalState, vm_op::VmOp},
        },
        test_support::test_config,
    };

    /// A fresh, uncommitted `EfunContext` whose call stack holds one real
    /// frame: `load_object` records the caller's debug span, so `frame()`
    /// must resolve.
    /// A `/caller` process with `function`'s frame on the stack.
    fn calling_frame(function: ProgramFunction) -> (TaskContext, CallStack<10>) {
        let (tx, _rx) = tokio::sync::mpsc::channel::<VmOp>(128);
        let global_state = GlobalState::new(test_config(), tx);
        let program = ProgramBuilder::default()
            .filename(LpcPath::InGame("/caller".into()))
            .build()
            .expect("program builder");
        let process = Arc::new(Process::new(program));
        let task_context = TaskContext::new(Arc::new(global_state), process.clone(), None);

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

    fn efun_context() -> (TaskContext, CallStack<10>) {
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
        calling_frame(function)
    }

    /// The arguments come from the calling frame's list: a register and a
    /// constant.
    #[test]
    fn at_call_reads_the_calling_frames_list() {
        use lpc_rs_asm::instruction::{Arg, ArgList};
        use lpc_rs_core::register::Register;
        use lpc_rs_function_support::constant::LpcConstant;

        let prototype = FunctionPrototypeBuilder::default()
            .name("caller")
            .filename(Arc::new(LpcPath::InGame("/caller".into())))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let mut function = ProgramFunction::new(prototype, 1);
        function.constants = vec![LpcConstant::Int(5)];
        function.arg_lists = vec![vec![
            Arg::Value(Register(1).as_local()),
            Arg::Value(Register(0).as_constant()),
        ]];
        let (task_context, mut stack) = calling_frame(function);
        stack.last_mut().unwrap().registers[1] = LpcRef::from(7);

        let ctx =
            EfunContext::at_call(&mut stack, &task_context, Efun::implode, ArgList(0)).unwrap();

        assert_eq!(ctx.arg_count(), 2);
        assert_eq!(*ctx.arg(0), LpcRef::from(7));
        assert_eq!(*ctx.arg(1), LpcRef::from(5));
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
        let ctx = EfunContext::new(&mut stack, &task_context, Efun::this_object);

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
        use crate::interpreter::efun::call_efun_sync;

        let (task_context, mut stack) = efun_context();
        let mut ctx = EfunContext::new(&mut stack, &task_context, Efun::this_object);
        assert!(call_efun_sync(Efun::this_object, &mut ctx).is_some());
        assert!(call_efun_sync(Efun::clone_object, &mut ctx).is_none());
    }

    #[test]
    fn an_int_result_lands_in_the_calling_frames_register_zero() {
        let (task_context, mut stack) = efun_context();
        let mut ctx = EfunContext::new(&mut stack, &task_context, Efun::this_object);
        ctx.return_efun_result(LpcRef::from(7));
        drop(ctx);
        assert_eq!(stack.last().unwrap().registers[0], LpcRef::from(7));
    }

    #[test]
    fn an_int_result_replaces_a_register_holding_another_type() {
        let (task_context, mut stack) = efun_context();
        stack.last_mut().unwrap().registers[0] = LpcString::from("was a string").into();
        let mut ctx = EfunContext::new(&mut stack, &task_context, Efun::this_object);
        ctx.return_efun_result(LpcRef::from(true));
        drop(ctx);
        assert_eq!(stack.last().unwrap().registers[0], LpcRef::from(1));
    }

    #[test]
    fn a_non_int_result_lands_in_the_calling_frames_register_zero() {
        let (task_context, mut stack) = efun_context();
        let mut ctx = EfunContext::new(&mut stack, &task_context, Efun::this_object);
        ctx.return_efun_result(LpcString::from("result").into());
        drop(ctx);
        assert_eq!(
            stack.last().unwrap().registers[0],
            LpcRef::from(LpcString::from("result"))
        );
    }

    /// With no frame under the efun, the result is handed to the task.
    #[test]
    fn a_result_with_no_calling_frame_is_the_tasks() {
        let (task_context, mut stack) = efun_context();
        let owner = stack.pop().unwrap().process;
        let mut ctx = EfunContext::fired(
            &mut stack,
            &task_context,
            Efun::this_object,
            vec![],
            owner,
            None,
        );
        ctx.return_efun_result(LpcRef::from(3));
        assert_eq!(ctx.finish(Ok(())).unwrap(), Some(LpcRef::from(3)));
    }

    #[test]
    fn write_ref_to_an_index_with_no_cell_is_a_runtime_bug() {
        let (task_context, mut stack) = efun_context();
        let ctx = EfunContext::new(&mut stack, &task_context, Efun::this_object);
        let err = ctx.write_ref(2, LpcRef::from(1)).unwrap_err().to_string();
        assert!(
            err.contains("argument 2 of `this_object` is not a by-reference argument"),
            "{err}"
        );
    }

    /// The calling frame's file is the program.
    #[test]
    fn calling_program_is_the_file_of_the_calling_frame() {
        let (task_context, mut stack) = efun_context();
        let ctx = EfunContext::new(&mut stack, &task_context, Efun::this_object);
        assert_eq!(ctx.calling_program().as_str(), Some("/caller"));
    }

    /// An efun fired through a pointer runs as the pointer's owner and
    /// reports the pointer's writer, not the frame under it.
    #[test]
    fn a_fired_efun_is_the_owners_with_the_pointers_origin() {
        let (task_context, mut stack) = efun_context();
        let owner = Arc::new(Process::new(
            ProgramBuilder::default()
                .filename(LpcPath::InGame("/owner".into()))
                .build()
                .unwrap(),
        ));
        let origin = Arc::new(LpcPath::InGame("/writer.c".into()));
        let ctx = EfunContext::fired(
            &mut stack,
            &task_context,
            Efun::this_object,
            vec![],
            owner.clone(),
            Some(origin),
        );
        assert!(Arc::ptr_eq(ctx.process(), &owner));
        assert_eq!(ctx.calling_program().as_str(), Some("/writer.c"));
    }

    /// An efun fired as a task's entry has no LPC caller.
    #[test]
    fn calling_program_is_zero_without_an_lpc_frame() {
        let (task_context, mut stack) = efun_context();
        let owner = stack.pop().unwrap().process;
        let ctx = EfunContext::fired(
            &mut stack,
            &task_context,
            Efun::this_object,
            vec![],
            owner,
            None,
        );
        assert!(ctx.calling_program().is_null());
    }
}
