use std::{path::PathBuf, sync::Arc};

use async_recursion::async_recursion;
use itertools::Itertools;
use lpc_rs_core::{lpc_path::LpcPath, register::RegisterVariant};
use lpc_rs_errors::{Result, lpc_bug};
use tracing::{instrument, trace};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        lpc_array::LpcArray,
        lpc_mapping::LpcMapping,
        lpc_ref::{LpcRef, NULL},
        process::Process,
        task::{Task, get_location},
        task_context::{ObjectLookup, TaskContext},
    },
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(level = "debug", skip_all)]
    #[inline]
    #[async_recursion]
    pub(crate) async fn handle_call_other(
        &mut self,
        receiver: RegisterVariant,
        name_location: RegisterVariant,
    ) -> Result<()> {
        // set up result_ref in a block, as `registers` is a long-lived reference that
        // doesn't work as mutable, but needs to be written to at the very end.
        let result_ref = {
            // figure out which function we're calling
            let receiver_ref = &*get_location(&self.stack, &self.context.txn, receiver)?;
            let name_ref = &*get_location(&self.stack, &self.context.txn, name_location)?;

            let Ok(function_name) = name_ref.with_string(|s| s.clone()) else {
                let str = format!("Invalid name passed to `call_other`: {}", name_ref);
                return Err(self.runtime_error(str));
            };

            trace!("Calling call_other: {}->{}", receiver_ref, function_name);

            let args = self
                .args
                .iter()
                .map(|i| get_location(&self.stack, &self.context.txn, *i).map(|r| r.into_owned()))
                .collect::<Result<Vec<_>>>()?;

            let function_name = Arc::new(function_name);

            match &receiver_ref {
                LpcRef::String(_) | LpcRef::Object(_) => {
                    Self::resolve_result(receiver_ref, &*function_name, &args, &self.context)
                        .await?
                }
                LpcRef::Array(_) => {
                    let mut refs = receiver_ref
                        .with_array(&self.context.txn, |a| a.iter().cloned().collect_vec())?;

                    for lpc_ref in &mut refs {
                        let ctx = &self.context;

                        let result =
                            Self::resolve_result(lpc_ref, &*function_name, &args, ctx).await?;

                        *lpc_ref = result;
                    }

                    LpcRef::Array(self.context.txn.with(|t| t.mint_array(LpcArray::new(refs))))
                }
                LpcRef::Mapping(_) => {
                    let mut map = receiver_ref.with_mapping(&self.context.txn, |m| {
                        m.iter().map(|(k, v)| (k.clone(), v.clone())).collect_vec()
                    })?;

                    for (_key_ref, value_ref) in map.iter_mut() {
                        let result =
                            Self::resolve_result(value_ref, &*function_name, &args, &self.context)
                                .await?;

                        *value_ref = result;
                    }

                    LpcRef::Mapping(
                        self.context
                            .txn
                            .with(|t| t.mint_mapping(LpcMapping::new(map.into_iter().collect()))),
                    )
                }
                _ => {
                    return Err(self.runtime_error(format!(
                        "What are you trying to call `{function_name}` on?"
                    )));
                }
            }
        };

        let registers = &mut self.stack.current_frame_mut()?.registers;
        registers[0] = result_ref;

        Ok(())
    }

    #[async_recursion]
    async fn resolve_result<T>(
        receiver_ref: &LpcRef,
        function_name: T,
        args: &[LpcRef],
        task_context: &TaskContext,
    ) -> Result<LpcRef>
    where
        T: AsRef<str> + Send + Sync,
    {
        let resolved = Task::<MAX_CALL_STACK_SIZE>::resolve_call_other_receiver(
            receiver_ref,
            function_name.as_ref(),
            task_context,
        )
        .await?;

        if let Some(receiver) = resolved {
            let new_context = task_context.clone().with_process(receiver.clone());
            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(new_context);

            // unwrap() is ok because resolve_call_other_receiver() checks
            // for the function's presence.
            let function = receiver
                .program
                .lookup_function(function_name.as_ref())
                .unwrap()
                .clone();

            let result = if function.public() {
                let max_execution_time = task_context.config().max_execution_time;
                task.timed_eval(function, args, max_execution_time).await?;

                let Some(r) = task.context.into_result() else {
                    return Err(lpc_bug!(
                        "resolve_result finished the task, but it has no result? wtf."
                    ));
                };

                r
            } else {
                NULL
            };

            Ok(result)
        } else {
            Ok(NULL)
        }
    }

    #[instrument(level = "debug", skip_all)]
    async fn resolve_call_other_receiver<T>(
        receiver_ref: &LpcRef,
        name: T,
        context: &TaskContext,
    ) -> Result<Option<Arc<Process>>>
    where
        T: AsRef<str>,
    {
        let process = match receiver_ref {
            LpcRef::String(_) => {
                let path = receiver_ref.with_string(|s| LpcPath::InGame(PathBuf::from(s)))?;

                match context.find_object(&path) {
                    ObjectLookup::Found(proc) => proc,
                    // A destructed object: don't create (that would resurrect
                    // it); the call below short-circuits.
                    ObjectLookup::Removed => return Ok(None),
                    // NotCreated: create-on-miss, transactionally.
                    ObjectLookup::NotCreated => {
                        let process = context.compile_process(&path).await?;
                        context.insert_process_transactional(&process);
                        process
                    }
                }
            }
            LpcRef::Object(_) => match receiver_ref.live_object(context.txn()) {
                Some(proc) => proc,
                None => return Ok(None),
            },
            _ => return Ok(None),
        };

        // If uninitialized, it's time to set that up. Note that we do this regardless
        // of whether the function exists or not, because this is a primary way of
        // initializing objects. If you've ever seen a call_other to some knowingly
        // undefined function in old lib code, this is why.
        let result = if !process.is_initialized(context.txn()) {
            let ctx = context.clone().with_process(process);
            Self::initialize_process(ctx).await?.context.process
        } else {
            process
        };

        // Only switch the process if there's actually a function to
        // call by this name on the other side.
        Ok(result.program.contains_function(name).then_some(result))
    }
}
