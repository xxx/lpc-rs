use async_recursion::async_recursion;
use lpc_rs_asm::instruction::{ArgList, Instruction};
use lpc_rs_core::{
    LpcIntInner,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::lpc_error;
use lpc_rs_utils::lpc_string::LpcString;
use thin_vec::ThinVec;
use tracing::{error, instrument, trace, warn};

use crate::interpreter::{
    efun::Efun,
    lpc_array::LpcArray,
    lpc_int::LpcInt,
    lpc_ref::{LpcRef, NULL, int_div, int_rem, int_shl, int_shr},
    task::{CatchPoint, Task, advance::Advance, bump_in_location, get_location, set_location},
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

/// What [`Task::step`] did with one instruction.
enum Step {
    /// Go on to the next instruction.
    Next,
    /// The stack is empty.
    Halt,
    /// [`Task::dispatch`] finishes the instruction.
    Await(AsyncCall),
}

/// The instructions that await, each because it can start a nested task.
pub(crate) enum AsyncCall {
    /// An efun that can suspend, with the calling instruction's list.
    Efun(Efun, ArgList),
    FunctionPointer(RegisterVariant, ArgList),
    Other(RegisterVariant, RegisterVariant, ArgList),
    /// The top frame's pending call needs the async arm.
    Pending,
}

/// Instructions between yields to the runtime.
const SLICE: u32 = 1000;

/// What ended a run of [`Task::step`]s.
pub(crate) enum Slice {
    /// The budget ran out.
    Budget,
    /// The stack is empty.
    Halt,
    /// [`Task::dispatch`] finishes the instruction.
    Await(AsyncCall),
}

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// Resume execution of a New or Paused Task. Assumes the stack has already been set up
    #[instrument(skip_all)]
    #[async_recursion]
    pub async fn resume(&mut self) -> lpc_rs_errors::Result<()> {
        let mut budget = SLICE;

        loop {
            let result = match self.run_slice(&mut budget) {
                Ok(Slice::Budget) => {
                    // Ensure infinite loops and the like don't monopolize the runtime.
                    budget = SLICE;
                    tokio::task::yield_now().await;
                    continue;
                }
                Ok(Slice::Halt) => break,
                Ok(Slice::Await(call)) => self.dispatch(call).await,
                Err(e) => Err(e),
            };

            if let Err(e) = result {
                if e.is_bug() {
                    error!("{}", e.diagnostic_string());
                }

                // `catch()` does not resume from a broken driver invariant.
                if !e.is_bug() && !self.catch_points.is_empty() {
                    self.catch_error(e)?;
                } else {
                    let stack_trace = self.stack.stack_trace();
                    return Err(e.with_stack_trace(stack_trace));
                }
            }
        }

        assert!(self.stack.is_empty());
        Ok(())
    }

    /// Step until an instruction awaits, the stack empties, or `budget` hits zero.
    ///
    /// Keep the count out of the `resume` future: there it was loaded and
    /// stored on every instruction.
    pub(crate) fn run_slice(&mut self, budget: &mut u32) -> lpc_rs_errors::Result<Slice> {
        let mut left = *budget;
        let slice = loop {
            if left == 0 {
                break Ok(Slice::Budget);
            }
            left -= 1;
            match self.step() {
                Ok(Step::Next) => {}
                Ok(Step::Halt) => break Ok(Slice::Halt),
                Ok(Step::Await(call)) => break Ok(Slice::Await(call)),
                Err(e) => break Err(e),
            }
        };
        *budget = left;
        slice
    }

    /// Finish an instruction that awaits.
    async fn dispatch(&mut self, call: AsyncCall) -> lpc_rs_errors::Result<()> {
        match call {
            AsyncCall::Efun(efun, list) => self.prepare_and_call_efun(efun, list).await,
            AsyncCall::FunctionPointer(location, list) => self.handle_call_fp(location, list).await,
            AsyncCall::Other(receiver, name, list) => {
                self.handle_call_other(receiver, name, list).await
            }
            AsyncCall::Pending => self.continue_pending().await,
        }
    }

    /// Evaluate the instruction at the current value of the program counter.
    ///
    /// Not `async`: as an `async fn` this was a 2.3 KiB future copied, polled
    /// and dropped on every instruction. Inlined into `run_slice` explicitly:
    /// left to LLVM it became a call per instruction once the doors grew.
    #[inline(always)]
    #[instrument(level = "debug", skip_all)]
    fn step(&mut self) -> lpc_rs_errors::Result<Step> {
        if self.stack.is_empty() {
            return Ok(Step::Halt);
        }

        let instruction = {
            let frame = match self.stack.current_frame_mut() {
                Ok(x) => x,
                Err(_) => {
                    warn!("Expected to get an instruction, but there are no more frames.");

                    return Ok(Step::Halt);
                }
            };

            let Some(instruction) = frame.instruction() else {
                warn!("No more instructions. Missing Ret instruction?");

                return Ok(Step::Halt);
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
                self.binary_operation(r1, r2, r3, |x, y| Some(x & y), |x, y, _| x.bitand(y))?;
            }
            Instruction::BitwiseNot(r1, r2) => {
                self.unary_operation(r1, r2, |x, _| x.bitnot())?;
            }
            Instruction::Call(name, list) => self.handle_call(name, list)?,
            Instruction::CallEfun(index, list) => {
                let Some(efun) = Efun::from_index(usize::from(index)) else {
                    return Err(self
                        .runtime_bug(format!("`CallEfun` index {index} is past the efun table")));
                };
                // An efun that never suspends runs with no future built.
                if efun.suspends() {
                    return Ok(Step::Await(AsyncCall::Efun(efun, list)));
                }
                if let Advance::Suspends = self.call_efun_now(efun, list)? {
                    return Ok(Step::Await(AsyncCall::Pending));
                }
            }
            Instruction::CallFp(location, list) => {
                // A pointer into a resident object is called with no future built.
                if let Some(call) = self.call_fp_local(location, list)? {
                    return Ok(Step::Await(call));
                }
            }
            Instruction::CallOther(receiver, name, list) => {
                // A receiver, or every remaining collection element, that
                // needs no loading is called with no future built.
                if !self.call_other_resident(receiver, name, list)? {
                    // A collection installs pending before this check; a lone receiver never does.
                    let call = if self.stack.current_frame()?.pending.is_some() {
                        AsyncCall::Pending
                    } else {
                        AsyncCall::Other(receiver, name, list)
                    };
                    return Ok(Step::Await(call));
                }
            }
            Instruction::CallSimulEfun(name, list) => {
                self.handle_call_simul_efun(name, list)?;
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
                let txn = &self.context.txn;
                let frame = self.stack.current_frame_mut()?;
                let new_ref = frame.get_location(txn, r1)?.into_owned();
                frame.set_location(txn, r2, new_ref)?;
            }
            Instruction::Dec(r1) => {
                bump_in_location(&mut self.stack, &self.context.txn, r1, -1)?;
            }
            Instruction::Cmp(kind, r1, r2, r3) => self.compare_into(kind, r1, r2, r3)?,
            Instruction::FunctionPtrConst {
                location,
                receiver,
                name,
            } => {
                let result = self.handle_functionptrconst(location, receiver, name);
                consumed(&mut self.partial_args, result)?;
            }
            Instruction::Div(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, int_div, |x, y, _| x.div(y))?;
            }
            Instruction::Mod(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, int_rem, |x, y, _| x.rem(y))?;
            }
            Instruction::Inc(r1) => {
                bump_in_location(&mut self.stack, &self.context.txn, r1, 1)?;
            }
            Instruction::Jcmp(kind, r1, r2, address) => {
                if self.holds(kind, r1, r2)? {
                    self.stack.current_frame_mut()?.set_pc(address);
                }
            }
            Instruction::Jmp(address) => {
                let frame = self.stack.current_frame_mut()?;
                frame.set_pc(address);
            }
            Instruction::Jncmp(kind, r1, r2, address) => {
                if !self.holds(kind, r1, r2)? {
                    self.stack.current_frame_mut()?.set_pc(address);
                }
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
                self.binary_operation(
                    r1,
                    r2,
                    r3,
                    |x, y| Some(x.wrapping_add(y)),
                    |x, y, txn| x.add(y, txn),
                )?;
            }
            Instruction::MapConst(location) => {
                let result = self.handle_mapconst(location);
                consumed(&mut self.array_items, result)?;
            }
            Instruction::Mul(r1, r2, r3) => {
                self.binary_operation(
                    r1,
                    r2,
                    r3,
                    |x, y| Some(x.wrapping_mul(y)),
                    |x, y, _| x.mul(y),
                )?;
            }
            Instruction::Sub(r1, r2, r3) => {
                self.binary_operation(
                    r1,
                    r2,
                    r3,
                    |x, y| Some(x.wrapping_sub(y)),
                    |x, y, txn| x.sub(y, txn),
                )?;
            }
            Instruction::NewUpvalue(location) => {
                self.stack.current_frame_mut()?.new_upvalue(location)?;
            }
            Instruction::Not(r1, r2) => {
                self.unary_operation(r1, r2, |x, txn| Ok(x.not(txn)))?;
            }
            Instruction::Or(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| Some(x | y), |x, y, _| x.bitor(y))?;
            }
            Instruction::PopulateArgv(r, num_args, num_locals) => {
                let frame = self.stack.current_frame()?;
                // The extras sit past the locals, where `store_arg` put them.
                let first = num_args + num_locals + 1;
                let extras = frame.called_with_num_args.saturating_sub(num_args);
                let refs = (0..extras)
                    .map(|j| {
                        frame
                            .get_location(&self.context.txn, Register(first + j).as_local())
                            .map(std::borrow::Cow::into_owned)
                    })
                    .collect::<lpc_rs_errors::Result<Vec<_>>>()?;

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
            Instruction::PushArrayItem(r1) => {
                self.array_items.push(r1);
            }
            Instruction::PushPartialArg(r) => {
                self.partial_args.push(r);
            }
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
                self.pop_frame()?;

                // halt at the end of all input
                if self.stack.is_empty() {
                    return Ok(Step::Halt);
                }
                // A return into a frame with a call in flight.
                if self.stack.current_frame()?.pending.is_some()
                    && matches!(self.advance_pending(true)?, Advance::Suspends)
                {
                    return Ok(Step::Await(AsyncCall::Pending));
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
                        lpc_ref.drop_dead_keys(txn)?;
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
                self.binary_operation(r1, r2, r3, |x, y| Some(int_shl(x, y)), |x, y, _| x.shl(y))?;
            }
            Instruction::Shr(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| Some(int_shr(x, y)), |x, y, _| x.shr(y))?;
            }
            Instruction::Xor(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| Some(x ^ y), |x, y, _| x.bitxor(y))?;
            }
        }

        Ok(Step::Next)
    }
}
