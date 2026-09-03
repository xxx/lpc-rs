use std::{path::PathBuf, sync::Arc};

use lpc_rs_core::{
    LpcIntInner, function_receiver::FunctionReceiver, lpc_path::LpcPath, register::RegisterVariant,
};
use lpc_rs_utils::lpc_string::LpcString;
use thin_vec::ThinVec;
use tracing::{instrument, trace};
use ustr::Ustr;

use crate::interpreter::{
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
    lpc_array::LpcArray,
    lpc_int::LpcInt,
    lpc_ref::{LpcRef, NULL},
    stm::MergeOp,
    task::{Task, get_location, set_location},
    task_context::ObjectLookup,
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) fn handle_aconst(&mut self, location: RegisterVariant) -> lpc_rs_errors::Result<()> {
        let items = &self.array_items;
        let vars = items
            .iter()
            .map(|i| get_location(&self.stack, &self.context.txn, *i).map(|i| i.into_owned()))
            .collect::<lpc_rs_errors::Result<Vec<_>>>()?;
        let new_ref = LpcRef::Array(self.context.txn.with(|t| t.mint_array(LpcArray::new(vars))));

        set_location(&mut self.stack, &self.context.txn, location, new_ref)
    }

    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) fn handle_functionptrconst(
        &mut self,
        location: RegisterVariant,
        receiver: FunctionReceiver,
        func_name: Ustr,
    ) -> lpc_rs_errors::Result<()> {
        let address = match receiver {
            FunctionReceiver::Efun => FunctionAddress::Efun(func_name),
            FunctionReceiver::SimulEfun => FunctionAddress::SimulEfun(func_name),
            FunctionReceiver::Dynamic => FunctionAddress::Dynamic(func_name),
            FunctionReceiver::Local => {
                let frame = self.stack.current_frame()?;
                let process = frame.process.clone();

                let func = {
                    let Some(func) = process.program.lookup_function(func_name) else {
                        return Err(self.runtime_error(format!(
                            "Unable to find function `{}` in local process `{}`.",
                            func_name,
                            process.filename()
                        )));
                    };

                    func.clone()
                };

                FunctionAddress::Local(Arc::downgrade(&process), func)
            }
            FunctionReceiver::Var(location) => {
                let receiver_ref = &*get_location(&self.stack, &self.context.txn, location)?;
                let process = match receiver_ref {
                    LpcRef::Object(weak_process) => {
                        let Some(process) = weak_process.upgrade() else {
                            return Err(self.runtime_error("called object is no longer available"));
                        };

                        process
                    }
                    LpcRef::String(_) => {
                        let path_str = receiver_ref
                            .with_string(|s| s.to_string())
                            .unwrap_or_default();
                        let path = LpcPath::InGame(PathBuf::from(path_str.as_str()));

                        match self.context.find_object(&path) {
                            ObjectLookup::Found(process) => process,
                            // A miss (removed, or never created) is a runtime
                            // error: this site does not create-on-miss.
                            ObjectLookup::Removed | ObjectLookup::NotCreated => {
                                return Err(self.runtime_error(format!(
                                    "Unable to find object `{}`.",
                                    path_str
                                )));
                            }
                        }
                    }
                    _ => {
                        return Err(self.runtime_error(format!(
                            "Unable to find the receiver for function `{}`.",
                            func_name
                        )));
                    }
                };

                let Some(func) = process.program.lookup_function(func_name) else {
                    return Err(self.runtime_error(format!(
                        "Unable to find function `{}` in remote process `{}`.",
                        func_name,
                        process.filename()
                    )));
                };
                // Visibility is decided when the pointer is taken; a pointer fires anywhere.
                if !func.public() && !Arc::ptr_eq(&process, &self.stack.current_frame()?.process) {
                    return Err(self.runtime_error(format!(
                        "cannot take a pointer to {} function `{}` of `{}`",
                        func.prototype.flags.visibility(),
                        func_name,
                        process.filename()
                    )));
                }

                FunctionAddress::Local(Arc::downgrade(&process), func.clone())
            }
        };

        let mut partial_args = self
            .partial_args
            .iter()
            .map(|arg| {
                arg.map(|register| {
                    Ok(get_location(&self.stack, &self.context.txn, register)?.into_owned())
                })
                .transpose()
            })
            .collect::<lpc_rs_errors::Result<ThinVec<Option<LpcRef>>>>()?;
        // A dynamic receiver is the pointer's first argument, bound at call time.
        if matches!(address, FunctionAddress::Dynamic(_)) {
            partial_args.insert(0, None);
        }

        let frame = self.stack.current_frame()?;
        // Only a closure continues its creator's capture numbering.
        let upvalue_ptrs = match &address {
            FunctionAddress::Local(_, func) if func.is_closure() => frame.upvalue_ptrs.clone(),
            _ => ThinVec::new(),
        };
        let fp = FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&frame.process))
            .address(address)
            .partial_args(partial_args)
            .upvalue_ptrs(upvalue_ptrs)
            .origin(Some(frame.function.prototype.filename.clone()))
            .build()
            .unwrap();

        let new_ref = fp.into();

        set_location(&mut self.stack, &self.context.txn, location, new_ref)
    }

    // #[instrument(level = "debug", skip_all)]
    // fn capture_environment(&mut self) -> Result<Vec<Register>> {
    //     let frame = self.stack.current_frame_mut()?;
    //     let mut upvalues = self.context.upvalues().write();
    //
    //     trace!("ptrs: {:?}", frame.upvalue_ptrs);
    //     trace!("upvalues: {:?}", upvalues);
    //
    //     frame
    //         .upvalue_ptrs
    //         .iter()
    //         .map(|ptr| {
    //             let upvalue = upvalues
    //                 .get(ptr.index() as usize)
    //                 .cloned()
    //                 .unwrap_or_default();
    //             let new_index = RegisterSize::try_from(upvalues.insert(upvalue))?;
    //             Ok(Register(new_index))
    //         })
    //         .collect::<Result<Vec<Register>>>()
    // }

    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) fn handle_load(
        &mut self,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
        destination: RegisterVariant,
    ) -> lpc_rs_errors::Result<()> {
        let container_ref =
            get_location(&self.stack, &self.context.txn, container_loc)?.into_owned();
        let lpc_ref = get_location(&self.stack, &self.context.txn, index_loc)?.into_owned();

        match container_ref {
            LpcRef::Array(_) => {
                let to_store = container_ref
                    .with_array(&self.context.txn, |vec| {
                        if let LpcRef::Int(i) = lpc_ref {
                            let idx = if i.0 >= 0 {
                                i.0
                            } else {
                                vec.len() as LpcIntInner + i.0
                            };

                            if idx >= 0 {
                                match vec.get(idx as usize) {
                                    Some(v) => Ok(v.clone()),
                                    None => Err(self.array_index_error(idx, vec.len())),
                                }
                            } else {
                                Err(self.array_index_error(idx, vec.len()))
                            }
                        } else {
                            Err(self.array_index_error(lpc_ref, vec.len()))
                        }
                    })
                    .flatten()?;
                set_location(&mut self.stack, &self.context.txn, destination, to_store)
            }
            LpcRef::String(_) => {
                let string = container_ref
                    .with_string(|lpc_string| lpc_string.to_string())
                    .unwrap_or_default();

                if let LpcRef::Int(i) = lpc_ref {
                    let idx = if i.0 >= 0 {
                        i.0
                    } else {
                        string.len() as LpcIntInner + i.0
                    };

                    if idx >= 0
                        && let Some(v) = string.chars().nth(idx as usize)
                    {
                        set_location(
                            &mut self.stack,
                            &self.context.txn,
                            destination,
                            LpcRef::Int(LpcInt(v as LpcIntInner)),
                        )?;
                    } else {
                        set_location(&mut self.stack, &self.context.txn, destination, NULL)?;
                    }
                } else {
                    return Err(self.runtime_error(format!(
                        "Attempting to access index {} in a string of length {}",
                        lpc_ref,
                        string.len()
                    )));
                }

                Ok(())
            }
            LpcRef::Mapping(_) => {
                let key = lpc_ref.mapping_key(&self.context.txn);
                let var = container_ref
                    .with_mapping(&self.context.txn, |map| {
                        map.get(&key).cloned().unwrap_or(NULL)
                    })
                    .unwrap_or(NULL);

                set_location(&mut self.stack, &self.context.txn, destination, var)?;

                Ok(())
            }
            x => Err(self.runtime_error(format!("Invalid attempt to take index of `{}`", x))),
        }
    }

    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) fn handle_load_mapping_key(
        &mut self,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
        destination: RegisterVariant,
    ) -> lpc_rs_errors::Result<()> {
        let var = {
            let container_ref = &*get_location(&self.stack, &self.context.txn, container_loc)?;

            match container_ref {
                LpcRef::Mapping(_) => {
                    let lpc_ref = &*get_location(&self.stack, &self.context.txn, index_loc)?;
                    let index = match lpc_ref {
                        LpcRef::Int(i) => i.0,
                        _ => {
                            return Err(
                                self.runtime_error(format!("Invalid index type: {}", lpc_ref))
                            );
                        }
                    };

                    container_ref
                        .with_mapping(&self.context.txn, |map| {
                            if let Some((key, _)) = map.get_index(index as usize) {
                                key.clone()
                            } else {
                                NULL
                            }
                        })
                        .unwrap_or(NULL)
                }
                x => {
                    return Err(
                        self.runtime_error(format!("Invalid attempt to take index of `{}`", x))
                    );
                }
            }
        };

        set_location(&mut self.stack, &self.context.txn, destination, var)
    }

    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) fn handle_sconst(
        &mut self,
        location: RegisterVariant,
        value: Ustr,
    ) -> lpc_rs_errors::Result<()> {
        let lpc_string = LpcString::Static(value);

        trace!(?lpc_string, "Storing static string");

        let new_ref = lpc_string.into();

        set_location(&mut self.stack, &self.context.txn, location, new_ref)
    }

    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) fn handle_store(
        &mut self,
        value_loc: RegisterVariant,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
    ) -> lpc_rs_errors::Result<()> {
        let container = get_location(&self.stack, &self.context.txn, container_loc)?.into_owned();
        let index = &*get_location(&self.stack, &self.context.txn, index_loc)?;
        let array_idx = if let LpcRef::Int(i) = index { i.0 } else { 0 };

        match &container {
            LpcRef::Array(_) => {
                // The value read happens before the COW: the closure runs
                // under the transaction write lock and cannot re-enter it.
                let value = (*get_location(&self.stack, &self.context.txn, value_loc)?).clone();
                container.with_array_cow(&self.context.txn, |vec| {
                    let cell_len = vec.len();
                    let idx = if array_idx >= 0 {
                        array_idx
                    } else {
                        cell_len as LpcIntInner + array_idx
                    };
                    if !(idx >= 0 && (idx as usize) < cell_len) {
                        return Err(self.array_index_error(idx, cell_len));
                    }
                    vec[idx as usize] = value;
                    Ok(())
                })?;
                Ok(())
            }
            LpcRef::Mapping(cell) => {
                let value = get_location(&self.stack, &self.context.txn, value_loc)?.into_owned();
                let key = index.mapping_key(&self.context.txn);
                // A mapping cell only ever holds a mapping, so the insert
                // needs no type peek and tracks no read.
                self.context
                    .txn
                    .with(|t| t.merge(cell.id, MergeOp::MapInsert(key, value)));
                Ok(())
            }
            x => Err(self.runtime_error(format!("Invalid attempt to take index of `{}`", x))),
        }
    }
}
