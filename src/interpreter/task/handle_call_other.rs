use std::{path::PathBuf, sync::Arc};

use async_recursion::async_recursion;
use futures::future::join_all;
use indexmap::IndexMap;
use itertools::Itertools;
use thin_vec::ThinVec;
use lpc_rs_core::{lpc_path::LpcPath, register::RegisterVariant};
use lpc_rs_errors::{lpc_bug, Result};
use tracing::{instrument, trace};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    get_loc,
    interpreter::{
        into_lpc_ref::IntoLpcRef,
        lpc_array::LpcArray,
        lpc_mapping::LpcMapping,
        lpc_ref::{LpcRef, NULL},
        object_flags::ObjectFlags,
        process::Process,
        task::{get_location, Task},
        task_context::TaskContext,
    },
    util::process_builder::ProcessCreator,
};
use crate::interpreter::task::task_id::TaskId;

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(skip_all)]
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
            let receiver_ref = &*get_location(&self.stack, receiver)?;
            let name_ref = &*get_location(&self.stack, name_location)?;
            let pool_ref = if let LpcRef::String(r) = name_ref {
                r
            } else {
                let str = format!("Invalid name passed to `call_other`: {}", name_ref);
                return Err(self.runtime_error(str));
            };

            let function_name = pool_ref.clone();

            trace!(
                "Calling call_other: {}->{}",
                receiver_ref,
                function_name.read()
            );

            let args = self
                .args
                .iter()
                .map(|i| get_loc!(self, *i).map(|r| r.into_owned()))
                .collect::<Result<Vec<_>>>()?;

            let function_name = Arc::new(function_name.read().clone());

            match &receiver_ref {
                LpcRef::String(_) | LpcRef::Object(_) => {
                    Self::resolve_result(self.id, receiver_ref, &*function_name, &args, &self.context)
                        .await?
                }
                LpcRef::Array(r) => {
                    let mut refs = r.read().iter().cloned().collect_vec();

                    for lpc_ref in &mut refs {
                        let ctx = &self.context;

                        let result = Self::resolve_result(self.id, &lpc_ref, &*function_name, &args, ctx)
                            .await
                            .unwrap_or(NULL);

                        *lpc_ref = result;
                    }

                    LpcArray::new(refs).into_lpc_ref(self.context.memory())
                }
                LpcRef::Mapping(m) => {
                    let mut map = m
                        .read()
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect_vec();

                    for (_key_ref, value_ref) in map.iter_mut() {
                        let result = Self::resolve_result(self.id, value_ref, &*function_name, &args, &self.context)
                            .await
                            .unwrap_or(NULL);

                        *value_ref = result;
                    }

                    LpcMapping::new(map.into_iter().collect())
                        .into_lpc_ref(self.context.memory())
                }
                _ => {
                    return Err(self.runtime_error(format!(
                        "What are you trying to call `{function_name}` on?"
                    )))
                }
            }
        };

        let registers = &mut self.stack.current_frame_mut()?.registers;
        registers[0] = result_ref;

        Ok(())
    }

    #[async_recursion]
    async fn resolve_result<T>(
        task_id: TaskId,
        receiver_ref: &LpcRef,
        function_name: T,
        args: &[LpcRef],
        task_context: &TaskContext,
    ) -> Result<LpcRef>
    where
        T: AsRef<str> + Send + Sync,
    {
        let resolved = Task::<MAX_CALL_STACK_SIZE>::resolve_call_other_receiver(
            task_id,
            receiver_ref,
            function_name.as_ref(),
            task_context,
        )
        .await;

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
                    return Err(lpc_bug!("resolve_result finished the task, but it has no result? wtf."));
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

    #[instrument(skip_all)]
    async fn resolve_call_other_receiver<T>(
        task_id: TaskId,
        receiver_ref: &LpcRef,
        name: T,
        context: &TaskContext,
    ) -> Option<Arc<Process>>
    where
        T: AsRef<str>,
    {
        let process = match receiver_ref {
            LpcRef::String(s) => {
                let lookup = {
                    let r = s.read();
                    let str = r.to_str();
                    context.lookup_process(str)
                };

                if let Some(proc) = lookup {
                    proc
                } else {
                    let path = LpcPath::InGame(PathBuf::from(s.read().to_str()));
                    context.create_process_from_path(&path).await.ok()?
                }
            }
            LpcRef::Object(proc) => {
                let Some(proc) = proc.upgrade() else {
                    return None;
                };

                proc
            }
            _ => return None,
        };

        // If uninitialized, it's time to set that up. Note that we do this regardless
        // of whether the function exists or not, because this is a primary way of
        // initializing objects. If you've ever seen a call_other to some knowingly
        // undefined function in old lib code, this is why.
        let result = if !process.flags.test(ObjectFlags::Initialized) {
            let ctx = context.clone().with_process(process);
            let Ok(task) = Self::initialize_sub_process(task_id, ctx).await else {
                return None;
            };

            task.context.process
        } else {
            process
        };

        // Only switch the process if there's actually a function to
        // call by this name on the other side.
        if result.program.contains_function(name) {
            Some(result)
        } else {
            None
        }
    }
}
