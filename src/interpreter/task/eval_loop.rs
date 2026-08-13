use async_recursion::async_recursion;
use decorum::Total;
use indexmap::IndexMap;
use itertools::Itertools;
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::{LpcIntInner, RegisterSize};
use lpc_rs_errors::lpc_error;
use tracing::{instrument, trace, warn};

use crate::{
    interpreter::{
        efun::EFUN_FUNCTIONS,
        lpc_array::LpcArray,
        lpc_int::LpcInt,
        lpc_mapping::LpcMapping,
        lpc_ref::{LpcRef, NULL},
        lpc_string::LpcString,
        task::{
            CatchPoint, Task, apply_in_location, get_location, get_location_in_frame, set_location,
            task_state::TaskState,
        },
    },
    pop_frame,
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// Resume execution of a New or Paused Task. Assumes the stack has already been set up
    #[instrument(skip_all)]
    #[async_recursion]
    pub async fn resume(&mut self) -> lpc_rs_errors::Result<()> {
        self.state = TaskState::Running;

        let f = &self.stack.current_frame()?.function.clone();

        if f.prototype.is_efun() {
            // call the efun, then we're done with this Task
            self.prepare_and_call_efun(f.name()).await?;
        } else {
            let mut halted = false;

            let mut c = 0 as RegisterSize;

            while !halted {
                halted = match self.eval_one_instruction().await {
                    Ok(x) => x,
                    Err(mut e) => {
                        if !self.catch_points.is_empty() {
                            self.catch_error(e)?;

                            false
                        } else {
                            let stack_trace = self.stack.stack_trace();
                            return Err({
                                *e = e.with_stack_trace(stack_trace);
                                e
                            });
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
        self.state = TaskState::Complete;
        Ok(())
    }

    /// Evaluate the instruction at the current value of the program counter.
    /// This is the main interpretation function for the VM.
    ///
    /// # Returns
    ///
    /// A [`lpc_rs_errors::Result`], with a boolean indicating whether we are at the end of input
    #[instrument(skip_all)]
    #[inline]
    #[async_recursion]
    async fn eval_one_instruction(&mut self) -> lpc_rs_errors::Result<bool> {
        if self.stack.is_empty() {
            self.state = TaskState::Complete;

            return Ok(true);
        }

        let instruction = {
            let frame = match self.stack.current_frame_mut() {
                Ok(x) => x,
                Err(_) => {
                    self.state = TaskState::Error;

                    warn!("Expected to get an instruction, but there are no more frames.");

                    return Ok(true);
                }
            };

            let Some(instruction) = frame.instruction() else {
                self.state = TaskState::Error;

                warn!("No more instructions. Missing Ret instruction?");

                return Ok(true);
            };
            trace!("about to evaluate: {}", instruction);

            frame.inc_pc();

            instruction
        };

        match instruction {
            Instruction::AConst(location) => {
                self.handle_aconst(location)?;
            }
            Instruction::And(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.bitand(y))?;
            }
            Instruction::BitwiseNot(r1, r2) => {
                let frame = self.stack.current_frame().unwrap();
                let debug_span = frame.current_debug_span();
                let lpc_ref = &*get_location(&self.stack, r1)?;
                match lpc_ref.bitnot() {
                    Ok(result) => {
                        set_location(&mut self.stack, r2, result)?;
                    }
                    Err(mut e) => {
                        *e = e.with_span(debug_span);
                        return Err(e);
                    }
                }
            }
            Instruction::Call(name_idx) => {
                self.handle_call(name_idx).await?;
            }
            Instruction::CallEfun(name_idx) => {
                let process = self.stack.current_frame()?.process.clone();
                let (pf, name) = {
                    let (name, pf) = EFUN_FUNCTIONS.get_index(name_idx as usize).unwrap();

                    (pf.clone(), name)
                };

                let new_frame = self.prepare_new_call_frame(process, pf).await?;

                self.stack.push(new_frame)?;

                self.prepare_and_call_efun(name).await?;
            }
            Instruction::CallFp(location) => {
                self.handle_call_fp(location).await?;
            }
            Instruction::CallOther(receiver, name) => {
                self.handle_call_other(receiver, name).await?;
            }
            Instruction::CallSimulEfun(name_idx) => {
                self.handle_call_simul_efun(name_idx).await?;
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
            Instruction::ClearArgs => {
                self.args.clear();
            }
            Instruction::ClearArrayItems => {
                self.array_items.clear();
            }
            Instruction::ClearPartialArgs => {
                self.partial_args.clear();
            }
            Instruction::Copy(r1, r2) => {
                let new_ref = get_location(&self.stack, r1)?.into_owned();
                set_location(&mut self.stack, r2, new_ref)?;
            }
            Instruction::Dec(r1) => {
                apply_in_location(&mut self.stack, r1, |x| x.dec())?;
            }
            Instruction::EqEq(r1, r2, r3) => {
                let out = (get_location(&self.stack, r1)? == get_location(&self.stack, r2)?)
                    as LpcIntInner;

                set_location(&mut self.stack, r3, LpcRef::Int(out.into()))?;
            }
            Instruction::FConst(r, f) => {
                set_location(&mut self.stack, r, LpcRef::Float(f.into()))?;
            }
            Instruction::FunctionPtrConst {
                location,
                receiver,
                name_index,
            } => {
                self.handle_functionptrconst(location, receiver, name_index)?;
            }
            Instruction::Gt(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x > y)?;
            }
            Instruction::Gte(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x >= y)?;
            }
            Instruction::IAdd(r1, r2, r3) => {
                match get_location(&self.stack, r1)?.add(&*get_location(&self.stack, r2)?) {
                    Ok(result) => {
                        set_location(&mut self.stack, r3, result)?;
                    }
                    Err(mut e) => {
                        let frame = self.stack.current_frame()?;
                        *e = e.with_span(frame.current_debug_span());
                        return Err(e);
                    }
                }
            }
            Instruction::IConst(r, i) => {
                set_location(&mut self.stack, r, LpcRef::Int(i.into()))?;
            }
            Instruction::IConst0(r) => {
                set_location(&mut self.stack, r, NULL)?;
            }
            Instruction::IConst1(r) => {
                set_location(&mut self.stack, r, LpcRef::Int(1.into()))?;
            }
            Instruction::IDiv(r1, r2, r3) => {
                match get_location(&self.stack, r1)?.div(&*get_location(&self.stack, r2)?) {
                    Ok(result) => set_location(&mut self.stack, r3, result)?,
                    Err(mut e) => {
                        let frame = self.stack.current_frame()?;
                        *e = e.with_span(frame.current_debug_span());
                        return Err(e);
                    }
                }
            }
            Instruction::IMod(r1, r2, r3) => {
                match get_location(&self.stack, r1)?.rem(&*get_location(&self.stack, r2)?) {
                    Ok(result) => set_location(&mut self.stack, r3, result)?,
                    Err(mut e) => {
                        let frame = self.stack.current_frame()?;
                        *e = e.with_span(frame.current_debug_span());
                        return Err(e);
                    }
                }
            }
            Instruction::IMul(r1, r2, r3) => {
                match get_location(&self.stack, r1)?.mul(&*get_location(&self.stack, r2)?) {
                    Ok(result) => set_location(&mut self.stack, r3, result)?,
                    Err(mut e) => {
                        let frame = self.stack.current_frame()?;
                        *e = e.with_span(frame.current_debug_span());
                        return Err(e);
                    }
                }
            }
            Instruction::Inc(r1) => {
                apply_in_location(&mut self.stack, r1, |x| x.inc())?;
            }
            Instruction::ISub(r1, r2, r3) => {
                match get_location(&self.stack, r1)?.sub(&*get_location(&self.stack, r2)?) {
                    Ok(result) => set_location(&mut self.stack, r3, result)?,
                    Err(mut e) => {
                        let frame = self.stack.current_frame()?;
                        *e = e.with_span(frame.current_debug_span());
                        return Err(e);
                    }
                }
            }
            Instruction::Jmp(address) => {
                let frame = self.stack.current_frame_mut()?;
                frame.set_pc(address);
            }
            Instruction::Jnz(r1, address) => {
                let v = &*get_location(&self.stack, r1)?;

                // TODO: re-decide of 0.0 floats should match here and with Jz
                if v != &NULL && v != &LpcRef::Float(Total::from(0.0).into()) {
                    let frame = self.stack.current_frame_mut()?;
                    frame.set_pc(address);
                }
            }
            Instruction::Jz(r1, address) => {
                let v = &*get_location(&self.stack, r1)?;

                if v == &NULL || v == &LpcRef::Float(Total::from(0.0).into()) {
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
            Instruction::Lt(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x < y)?;
            }
            Instruction::Lte(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x <= y)?;
            }
            Instruction::MAdd(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.add(y))?;
            }
            Instruction::MapConst(r) => {
                let mut register_map = IndexMap::with_capacity(self.array_items.len() / 2);

                debug_assert!(
                    self.array_items.len().is_multiple_of(2),
                    "Odd number of items in `array` when creating a mapping constant"
                );
                for chunk in &self.array_items.iter().copied().chunks(2) {
                    let (key, value) = chunk.into_iter().collect_tuple().unwrap();
                    register_map.insert(
                        get_location(&self.stack, key)?.into_owned(),
                        get_location(&self.stack, value)?.into_owned(),
                    );
                }

                let new_ref = LpcMapping::new(register_map.into_iter().collect()).into();

                set_location(&mut self.stack, r, new_ref)?;
            }
            Instruction::MMul(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.mul(y))?;
            }
            Instruction::MSub(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.sub(y))?;
            }
            Instruction::Not(r1, r2) => {
                let matched = match &*get_location(&self.stack, r1)? {
                    LpcRef::Int(x) => LpcRef::Int(LpcInt((*x == 0) as LpcIntInner)),
                    LpcRef::Float(x) => LpcRef::Int(LpcInt((*x == 0.0) as LpcIntInner)),

                    // These rest always have a value at runtime.
                    // Any null / undefined values would be LpcRef::Ints, handled above.
                    LpcRef::String(_)
                    | LpcRef::Array(_)
                    | LpcRef::Mapping(_)
                    | LpcRef::Object(_)
                    | LpcRef::Function(_) => NULL,
                };

                set_location(&mut self.stack, r2, matched)?;
            }
            Instruction::NotEq(r1, r2, r3) => {
                let out = (get_location(&self.stack, r1)? != get_location(&self.stack, r2)?)
                    as LpcIntInner;

                set_location(&mut self.stack, r3, LpcRef::Int(LpcInt(out)))?;
            }
            Instruction::Or(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.bitor(y))?;
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
                            .map(|x| get_location_in_frame(frame, *x).map(|v| v.into_owned()))
                            .collect::<lpc_rs_errors::Result<Vec<_>>>()?
                    }
                };

                let new_ref = LpcArray::new(refs).into();

                set_location(&mut self.stack, r, new_ref)?;
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
            Instruction::PushArg(r) => self.args.push(r),
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
                    set_location(stack, r4, new_ref)?;

                    Ok(())
                };

                let get_new_value = |stack| -> lpc_rs_errors::Result<LpcRef> {
                    let lpc_ref = &*get_location(stack, r1)?;

                    match lpc_ref {
                        LpcRef::Array(_) => lpc_ref
                            .with_array(|vec| {
                                if vec.is_empty() {
                                    return Ok(LpcArray::new(vec![]).into());
                                }

                                let index1 = &*get_location(stack, r2)?;
                                let index2 = &*get_location(stack, r3)?;

                                if let (LpcRef::Int(start), LpcRef::Int(end)) = (&index1, &index2) {
                                    let (real_start, real_end) =
                                        resolve_range(start.0, end.0, vec.len());

                                    if real_start <= real_end {
                                        let slice = &vec[real_start..=real_end];
                                        let mut new_vec = vec![NULL; slice.len()];
                                        new_vec.clone_from_slice(slice);
                                        Ok(LpcArray::new(new_vec).into())
                                    } else {
                                        Ok(LpcArray::new(vec![]).into())
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

                                let index1 = &*get_location(stack, r2)?;
                                let index2 = &*get_location(stack, r3)?;

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
                    self.state = TaskState::Complete;
                    return Ok(true);
                }
            }
            Instruction::Sizeof(r1, r2) => {
                let lpc_ref = &*get_location(&self.stack, r1)?;

                let new_ref = match lpc_ref {
                    LpcRef::Array(_) => {
                        let l = lpc_ref.with_array(|a| a.len())?;

                        LpcRef::Int(LpcInt(l as LpcIntInner))
                    }
                    LpcRef::Mapping(_) => {
                        let l = lpc_ref.with_mapping(|m| m.len())?;

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

                set_location(&mut self.stack, r2, new_ref)?;
            }
            Instruction::Store(value_loc, container_loc, index_loc) => {
                // r2[r3] = r1;
                self.handle_store(value_loc, container_loc, index_loc)?;
            }
            Instruction::SConst(location, index) => {
                self.handle_sconst(location, index)?;
            }
            Instruction::Shl(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.shl(y))?;
            }
            Instruction::Shr(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.shr(y))?;
            }
            Instruction::Xor(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.bitxor(y))?;
            }
        }

        Ok(false)
    }
}
