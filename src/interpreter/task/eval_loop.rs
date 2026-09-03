use async_recursion::async_recursion;
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::{LpcIntInner, RegisterSize};
use lpc_rs_errors::lpc_error;
use lpc_rs_utils::lpc_string::LpcString;
use thin_vec::ThinVec;
use tracing::{error, instrument, trace, warn};

use crate::{
    interpreter::{
        efun::EFUN_FUNCTIONS,
        lpc_array::LpcArray,
        lpc_int::LpcInt,
        lpc_ref::{LpcRef, NULL},
        task::{Arg, CatchPoint, Task, bump_in_location, get_location, set_location},
    },
    pop_frame,
};

/// Empty a staging vector once its consuming instruction has run, whether or
/// not it succeeded — a `catch` would otherwise hand the stale entries to the
/// next consumer.
fn consumed<T, R>(
    staging: &mut ThinVec<T>,
    result: lpc_rs_errors::Result<R>,
) -> lpc_rs_errors::Result<R> {
    staging.clear();
    result
}

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// Resume execution of a New or Paused Task. Assumes the stack has already been set up
    #[instrument(skip_all)]
    #[async_recursion]
    pub async fn resume(&mut self) -> lpc_rs_errors::Result<()> {
        let f = &self.stack.current_frame()?.function.clone();

        if f.prototype.is_efun() {
            // call the efun, then we're done with this Task
            if let Err(e) = self.prepare_and_call_efun(f.name()).await {
                return Err(e.with_stack_trace(self.stack.stack_trace()));
            }
        } else {
            let mut halted = false;

            let mut c = 0 as RegisterSize;

            while !halted {
                halted = match self.eval_one_instruction().await {
                    Ok(x) => x,
                    Err(e) => {
                        if e.is_bug() {
                            error!("{}", e.diagnostic_string());
                        }

                        // `catch()` does not resume from a broken driver invariant.
                        if !e.is_bug() && !self.catch_points.is_empty() {
                            self.catch_error(e)?;

                            false
                        } else {
                            let stack_trace = self.stack.stack_trace();
                            return Err(e.with_stack_trace(stack_trace));
                        }
                    }
                };

                // Ensure infinite loops and the like don't monopolize the runtime.
                c += 1;
                if c == 1000 {
                    c = 0;
                    tokio::task::yield_now().await;
                }
            }
        }

        assert!(self.stack.is_empty());
        Ok(())
    }

    /// Evaluate the instruction at the current value of the program counter.
    /// This is the main interpretation function for the VM.
    ///
    /// # Returns
    ///
    /// A [`lpc_rs_errors::Result`], with a boolean indicating whether we are at the end of input
    // Not boxed: the recursion cycle is already broken by the boxed
    // futures on `timed_eval_seed` and `resume`.
    #[instrument(level = "debug", skip_all)]
    #[inline]
    async fn eval_one_instruction(&mut self) -> lpc_rs_errors::Result<bool> {
        if self.stack.is_empty() {
            return Ok(true);
        }

        let instruction = {
            let frame = match self.stack.current_frame_mut() {
                Ok(x) => x,
                Err(_) => {
                    warn!("Expected to get an instruction, but there are no more frames.");

                    return Ok(true);
                }
            };

            let Some(instruction) = frame.instruction() else {
                warn!("No more instructions. Missing Ret instruction?");

                return Ok(true);
            };
            trace!("about to evaluate: {}", instruction);

            frame.inc_pc();

            instruction
        };

        #[cfg(feature = "opcode-profile")]
        crate::interpreter::opcode_profile::record(&instruction);

        match instruction {
            Instruction::AConst(location) => {
                let result = self.handle_aconst(location);
                consumed(&mut self.array_items, result)?;
            }
            Instruction::And(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _| x.bitand(y))?;
            }
            Instruction::BitwiseNot(r1, r2) => {
                self.unary_operation(r1, r2, |x, _| x.bitnot())?;
            }
            Instruction::Call(name) => {
                let result = self.handle_call(name).await;
                consumed(&mut self.args, result)?;
            }
            Instruction::CallEfun(name_idx) => {
                let process = self.stack.current_frame()?.process.clone();
                let (pf, name) = {
                    let (name, pf) = EFUN_FUNCTIONS.get_index(name_idx as usize).unwrap();

                    (pf.clone(), name)
                };

                let new_frame = self.prepare_new_call_frame(process, pf).await;
                let new_frame = consumed(&mut self.args, new_frame)?;

                self.stack.push(new_frame)?;

                self.prepare_and_call_efun(name).await?;
            }
            Instruction::CallFp(location) => {
                let result = self.handle_call_fp(location).await;
                consumed(&mut self.args, result)?;
            }
            Instruction::CallOther(receiver, name) => {
                let result = self.handle_call_other(receiver, name).await;
                consumed(&mut self.args, result)?;
            }
            Instruction::CallSimulEfun(name) => {
                let result = self.handle_call_simul_efun(name).await;
                consumed(&mut self.args, result)?;
            }
            Instruction::CatchEnd => {
                self.catch_points.pop();
            }
            Instruction::CatchStart(r, address) => {
                let catch_point = CatchPoint {
                    frame_index: self.stack.len() - 1,
                    register: r,
                    address,
                };

                self.catch_points.push(catch_point);
            }
            Instruction::Copy(r1, r2) => {
                let new_ref = get_location(&self.stack, &self.context.txn, r1)?.into_owned();
                set_location(&mut self.stack, &self.context.txn, r2, new_ref)?;
            }
            Instruction::Dec(r1) => {
                bump_in_location(&mut self.stack, &self.context.txn, r1, -1)?;
            }
            Instruction::Cmp(kind, r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y, txn| x.compare(kind, y, txn))?;
            }
            Instruction::FunctionPtrConst {
                location,
                receiver,
                name,
            } => {
                let result = self.handle_functionptrconst(location, receiver, name);
                consumed(&mut self.partial_args, result)?;
            }
            Instruction::Div(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _| x.div(y))?;
            }
            Instruction::Mod(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _| x.rem(y))?;
            }
            Instruction::Inc(r1) => {
                bump_in_location(&mut self.stack, &self.context.txn, r1, 1)?;
            }
            Instruction::Jmp(address) => {
                let frame = self.stack.current_frame_mut()?;
                frame.set_pc(address);
            }
            Instruction::Jnz(r1, address) => {
                let v = &*get_location(&self.stack, &self.context.txn, r1)?;

                if v.is_truthy(&self.context.txn) {
                    let frame = self.stack.current_frame_mut()?;
                    frame.set_pc(address);
                }
            }
            Instruction::Jz(r1, address) => {
                let v = &*get_location(&self.stack, &self.context.txn, r1)?;

                if !v.is_truthy(&self.context.txn) {
                    let frame = self.stack.current_frame_mut()?;
                    frame.set_pc(address);
                }
            }
            Instruction::Load(container, index, destination) => {
                self.handle_load(container, index, destination)?;
            }
            Instruction::LoadMappingKey(container, index, destination) => {
                self.handle_load_mapping_key(container, index, destination)?;
            }
            Instruction::Add(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, txn| x.add(y, txn))?;
            }
            Instruction::MapConst(location) => {
                let result = self.handle_mapconst(location);
                consumed(&mut self.array_items, result)?;
            }
            Instruction::Mul(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _| x.mul(y))?;
            }
            Instruction::Sub(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, txn| x.sub(y, txn))?;
            }
            Instruction::NewUpvalue(location) => {
                self.stack.current_frame_mut()?.new_upvalue(location)?;
            }
            Instruction::Not(r1, r2) => {
                self.unary_operation(r1, r2, |x, txn| Ok(x.not(txn)))?;
            }
            Instruction::Or(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _| x.bitor(y))?;
            }
            Instruction::PopulateArgv(r, num_args, _num_locals) => {
                let frame = self.stack.current_frame()?;
                let arg_locations = &frame.arg_locations;
                let num_args = usize::from(num_args);
                let refs = {
                    if arg_locations.len() < num_args {
                        vec![]
                    } else {
                        let ellipsis_vars = &arg_locations[num_args..];
                        ellipsis_vars
                            .iter()
                            .map(|x| {
                                frame
                                    .get_location(&self.context.txn, *x)
                                    .map(std::borrow::Cow::into_owned)
                            })
                            .collect::<lpc_rs_errors::Result<Vec<_>>>()?
                    }
                };

                let new_ref =
                    LpcRef::Array(self.context.txn.with(|t| t.mint_array(LpcArray::new(refs))));

                set_location(&mut self.stack, &self.context.txn, r, new_ref)?;
            }
            Instruction::PopulateDefaults => {
                // let default_addresses = &self.defaults;
                let frame = self.stack.current_frame_mut()?;
                let func = &frame.function;
                let num_args = func.arity().num_args;
                let num_default_args = func.arity().num_default_args;
                // let non_default_args = num_args - num_default_args;
                let called_args = frame.called_with_num_args;
                let defaults_to_init = (num_args.saturating_sub(called_args)).min(num_default_args);

                let jump = num_default_args - defaults_to_init;
                frame.set_pc(frame.pc() + jump as usize);
            }
            Instruction::PushArg(r) => self.args.push(Arg::Value(r)),
            Instruction::PushArrayItem(r1) => {
                self.array_items.push(r1);
            }
            Instruction::PushPartialArg(r) => {
                self.partial_args.push(r);
            }
            Instruction::PushRef(r) => self.args.push(Arg::Ref(r)),
            Instruction::Range(r1, r2, r3, r4) => {
                // r4 = r1[r2..r3]

                let resolve_range = |start: i64, end: i64, len: usize| -> (usize, usize) {
                    let to_idx = |i: LpcIntInner| {
                        // We handle the potential overflow just below.
                        if i >= 0 {
                            i as usize
                        } else {
                            (len as LpcIntInner + i) as usize
                        }
                    };
                    let real_start = to_idx(start);
                    let mut real_end = to_idx(end);

                    if real_end >= len {
                        real_end = len - 1;
                    }

                    (real_start, real_end)
                };

                let return_value = |new_ref, stack| -> lpc_rs_errors::Result<()> {
                    set_location(stack, &self.context.txn, r4, new_ref)?;

                    Ok(())
                };

                let get_new_value = |stack| -> lpc_rs_errors::Result<LpcRef> {
                    let lpc_ref = &*get_location(stack, &self.context.txn, r1)?;

                    match lpc_ref {
                        LpcRef::Array(_) => lpc_ref
                            .with_array(&self.context.txn, |vec| {
                                if vec.is_empty() {
                                    return Ok(LpcRef::Array(
                                        self.context
                                            .txn
                                            .with(|t| t.mint_array(LpcArray::new(vec![]))),
                                    ));
                                }

                                let index1 = &*get_location(stack, &self.context.txn, r2)?;
                                let index2 = &*get_location(stack, &self.context.txn, r3)?;

                                if let (LpcRef::Int(start), LpcRef::Int(end)) = (&index1, &index2) {
                                    let (real_start, real_end) =
                                        resolve_range(start.0, end.0, vec.len());

                                    if real_start <= real_end {
                                        let slice = &vec[real_start..=real_end];
                                        let mut new_vec = vec![NULL; slice.len()];
                                        new_vec.clone_from_slice(slice);
                                        Ok(LpcRef::Array(
                                            self.context
                                                .txn
                                                .with(|t| t.mint_array(LpcArray::new(new_vec))),
                                        ))
                                    } else {
                                        Ok(LpcRef::Array(
                                            self.context
                                                .txn
                                                .with(|t| t.mint_array(LpcArray::new(vec![]))),
                                        ))
                                    }
                                } else {
                                    let frame = self.stack.current_frame()?;
                                    Err(lpc_error!(
                                        frame.current_debug_span(),
                                        "Invalid code was generated for a Range instruction.",
                                    ))
                                }
                            })
                            .flatten(),
                        LpcRef::String(_) => lpc_ref
                            .with_string(|string| {
                                if string.is_empty() {
                                    return Ok(LpcString::from("").into());
                                }

                                let index1 = &*get_location(stack, &self.context.txn, r2)?;
                                let index2 = &*get_location(stack, &self.context.txn, r3)?;

                                if let (LpcRef::Int(start), LpcRef::Int(end)) = (&index1, &index2) {
                                    let (real_start, real_end) =
                                        resolve_range(start.0, end.0, string.len());

                                    if real_start <= real_end {
                                        let len = real_end - real_start + 1;
                                        let new_string: String =
                                            string.chars().skip(real_start).take(len).collect();
                                        Ok(LpcString::from(new_string).into())
                                    } else {
                                        Ok(LpcString::from("").into())
                                    }
                                } else {
                                    let frame = self.stack.current_frame()?;
                                    Err(lpc_error!(
                                        frame.current_debug_span(),
                                        "Invalid code was generated for a Range instruction.",
                                    ))
                                }
                            })
                            .flatten(),
                        LpcRef::Float(_)
                        | LpcRef::Int(_)
                        | LpcRef::Mapping(_)
                        | LpcRef::Object(_)
                        | LpcRef::Function(_) => {
                            let frame = self.stack.current_frame()?;
                            Err(lpc_error!(
                                frame.current_debug_span(),
                                "Range's receiver isn't actually an array or string?",
                            ))
                        }
                    }
                };

                let new_ref = get_new_value(&self.stack)?;
                return_value(new_ref, &mut self.stack)?;
            }
            Instruction::Ret => {
                pop_frame!(self).map(|frame| {
                    trace!("Returning from function: {}", frame.function.name());
                });

                // halt at the end of all input
                if self.stack.is_empty() {
                    return Ok(true);
                }
                // A return into a frame mid-way through a collection `->`.
                if self.stack.current_frame()?.pending.is_some() {
                    self.advance_collection_call().await?;
                }
            }
            Instruction::Sizeof(r1, r2) => {
                let lpc_ref = &*get_location(&self.stack, &self.context.txn, r1)?;

                let new_ref = match lpc_ref {
                    LpcRef::Array(_) => {
                        let l = lpc_ref.with_array(&self.context.txn, |a| a.len())?;

                        LpcRef::Int(LpcInt(l as LpcIntInner))
                    }
                    LpcRef::Mapping(_) => {
                        let txn = &self.context.txn;
                        // The COW closure holds the transaction lock, so liveness is checked before it.
                        let dead = lpc_ref.with_mapping(txn, |m| {
                            m.keys()
                                .filter(|k| k.is_dead_object(txn))
                                .cloned()
                                .collect::<Vec<_>>()
                        })?;
                        if !dead.is_empty() {
                            lpc_ref.with_mapping_cow(txn, |m| {
                                m.retain(|k, _| !dead.contains(k));
                                Ok(())
                            })?;
                        }
                        let l = lpc_ref.with_mapping(txn, |m| m.len())?;

                        LpcRef::Int(LpcInt(l as LpcIntInner))
                    }
                    LpcRef::String(_) => {
                        let l = lpc_ref.with_string(|s| s.len())?;

                        LpcRef::Int(LpcInt(l as LpcIntInner))
                    }
                    LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::Object(_) | LpcRef::Function(_) => {
                        NULL
                    }
                };

                set_location(&mut self.stack, &self.context.txn, r2, new_ref)?;
            }
            Instruction::Store(value_loc, container_loc, index_loc) => {
                // r2[r3] = r1;
                self.handle_store(value_loc, container_loc, index_loc)?;
            }
            Instruction::Shl(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _| x.shl(y))?;
            }
            Instruction::Shr(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _| x.shr(y))?;
            }
            Instruction::Xor(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _| x.bitxor(y))?;
            }
        }

        Ok(false)
    }
}
