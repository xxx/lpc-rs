use std::sync::Arc;

use lpc_rs_core::{
    LpcIntInner, RegisterSize, function_receiver::FunctionReceiver, register::RegisterVariant,
};
use parking_lot::RwLock;
use thin_vec::ThinVec;
use tracing::{instrument, trace};
use ustr::ustr;

use crate::interpreter::{
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
    gc::unique_id::UniqueId,
    lpc_array::LpcArray,
    lpc_int::LpcInt,
    lpc_ref::{LpcRef, NULL},
    lpc_string::LpcString,
    task::{Task, get_location, set_location},
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(skip_all)]
    #[inline]
    pub(crate) fn handle_aconst(&mut self, location: RegisterVariant) -> lpc_rs_errors::Result<()> {
        let items = &self.array_items;
        let vars = items
            .iter()
            .map(|i| get_location(&self.stack, *i).map(|i| i.into_owned()))
            .collect::<lpc_rs_errors::Result<Vec<_>>>()?;
        let new_ref = LpcArray::new(vars).into();

        set_location(&mut self.stack, location, new_ref)
    }

    #[instrument(skip_all)]
    #[inline]
    pub(crate) fn handle_functionptrconst(
        &mut self,
        location: RegisterVariant,
        receiver: FunctionReceiver,
        name_idx: RegisterSize,
    ) -> lpc_rs_errors::Result<()> {
        let call_other = match receiver {
            FunctionReceiver::Var(_) | FunctionReceiver::Dynamic => true,
            FunctionReceiver::Local | FunctionReceiver::Efun | FunctionReceiver::SimulEfun => false,
        };

        let Some(func_name) = self
            .stack
            .current_frame()?
            .function
            .strings
            .get()
            .unwrap()
            .resolve(Self::index_symbol(name_idx))
        else {
            return Err(self.runtime_bug("Unable to find the name being pointed to."));
        };

        let address = match receiver {
            FunctionReceiver::Efun => FunctionAddress::Efun(ustr(func_name)),
            FunctionReceiver::SimulEfun => FunctionAddress::SimulEfun(ustr(func_name)),
            FunctionReceiver::Dynamic => FunctionAddress::Dynamic(ustr(func_name)),
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
                let receiver_ref = &*get_location(&self.stack, location)?;
                match receiver_ref {
                    LpcRef::Object(weak_process) => {
                        let Some(process) = weak_process.upgrade() else {
                            return Err(self.runtime_error("called object is no longer available"));
                        };

                        let func = {
                            let Some(func) = process.program.lookup_function(func_name) else {
                                return Err(self.runtime_error(format!(
                                    "Unable to find function `{}` in remote process `{}`.",
                                    func_name,
                                    process.filename()
                                )));
                            };

                            func.clone()
                        };
                        let weak_process = (*weak_process).clone();
                        FunctionAddress::Local(weak_process, func)
                    }
                    LpcRef::String(s) => {
                        let process = {
                            let path = s.read();

                            let Some(process) = self.context.lookup_process(&*path) else {
                                return Err(self
                                    .runtime_error(format!("Unable to find object `{}`.", path)));
                            };

                            process
                        };

                        let func = {
                            let Some(func) = process.program.lookup_function(func_name) else {
                                return Err(self.runtime_error(format!(
                                    "Unable to find function `{}` in remote process `{}`.",
                                    func_name,
                                    process.filename()
                                )));
                            };

                            func.clone()
                        };

                        FunctionAddress::Local(Arc::downgrade(&process), func)
                    }
                    _ => {
                        return Err(self.runtime_error(format!(
                            "Unable to find the receiver for function `{}`.",
                            func_name
                        )));
                    }
                }
            }
        };

        let partial_args = self
            .partial_args
            .iter()
            .map(|arg| {
                arg.map(|register| Ok(get_location(&self.stack, register)?.into_owned()))
                    .transpose()
            })
            .collect::<lpc_rs_errors::Result<ThinVec<Option<LpcRef>>>>()?;

        let frame = self.stack.current_frame()?;
        let fp = FunctionPtr {
            owner: Arc::downgrade(&frame.process),
            address,
            partial_args: RwLock::new(partial_args),
            call_other,
            // Function pointers inherit the current upvalue_ptrs
            upvalue_ptrs: frame.upvalue_ptrs.clone(),
            unique_id: UniqueId::new(),
        };

        let new_ref = fp.into();

        set_location(&mut self.stack, location, new_ref)
    }

    // #[instrument(skip_all)]
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

    #[instrument(skip_all)]
    #[inline]
    pub(crate) fn handle_load(
        &mut self,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
        destination: RegisterVariant,
    ) -> lpc_rs_errors::Result<()> {
        let container_ref = get_location(&self.stack, container_loc)?.into_owned();
        let lpc_ref = get_location(&self.stack, index_loc)?.into_owned();

        match container_ref {
            LpcRef::Array(vec_ref) => {
                let vec = vec_ref.read();

                if let LpcRef::Int(i) = lpc_ref {
                    let idx = if i.0 >= 0 {
                        i.0
                    } else {
                        vec.len() as LpcIntInner + i.0
                    };

                    if idx >= 0 {
                        if let Some(v) = vec.get(idx as usize) {
                            set_location(&mut self.stack, destination, v.clone())?;
                        } else {
                            return Err(self.array_index_error(idx, vec.len()));
                        }
                    } else {
                        return Err(self.array_index_error(idx, vec.len()));
                    }
                } else {
                    return Err(self.array_index_error(lpc_ref, vec.len()));
                }

                Ok(())
            }
            LpcRef::String(string_ref) => {
                let lock = string_ref.read();
                let string = lock.to_str();

                if let LpcRef::Int(i) = lpc_ref {
                    let idx = if i.0 >= 0 {
                        i.0
                    } else {
                        string.len() as LpcIntInner + i.0
                    };

                    if idx >= 0 {
                        if let Some(v) = string.chars().nth(idx as usize) {
                            set_location(
                                &mut self.stack,
                                destination,
                                LpcRef::Int(LpcInt(v as LpcIntInner)),
                            )?;
                        } else {
                            set_location(&mut self.stack, destination, NULL)?;
                        }
                    } else {
                        set_location(&mut self.stack, destination, NULL)?;
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
            LpcRef::Mapping(map_ref) => {
                let map = map_ref.read();

                let var = if let Some(v) = map.get(&lpc_ref) {
                    v.clone()
                } else {
                    NULL
                };

                set_location(&mut self.stack, destination, var)?;

                Ok(())
            }
            x => Err(self.runtime_error(format!("Invalid attempt to take index of `{}`", x))),
        }
    }

    #[instrument(skip_all)]
    #[inline]
    pub(crate) fn handle_load_mapping_key(
        &mut self,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
        destination: RegisterVariant,
    ) -> lpc_rs_errors::Result<()> {
        let var = {
            let container_ref = &*get_location(&self.stack, container_loc)?;
            let lpc_ref = &*get_location(&self.stack, index_loc)?;

            match container_ref {
                LpcRef::Mapping(map_ref) => {
                    let map = map_ref.read();

                    let index = match lpc_ref {
                        LpcRef::Int(i) => i.0,
                        _ => {
                            return Err(
                                self.runtime_error(format!("Invalid index type: {}", lpc_ref))
                            );
                        }
                    };

                    if let Some((key, _)) = map.get_index(index as usize) {
                        key.clone()
                    } else {
                        NULL
                    }
                }
                x => {
                    return Err(
                        self.runtime_error(format!("Invalid attempt to take index of `{}`", x))
                    );
                }
            }
        };

        set_location(&mut self.stack, destination, var)
    }

    #[instrument(skip_all)]
    #[inline]
    pub(crate) fn handle_sconst(
        &mut self,
        location: RegisterVariant,
        index: usize,
    ) -> lpc_rs_errors::Result<()> {
        let function_strings = self.stack.current_frame()?.function.strings.get();
        const MSG: &str = "the `strings` reference was never assigned to the function.";
        debug_assert!(function_strings.is_some(), "{}", MSG); // This is very bad if it happens.
        let Some(strings) = function_strings else {
            return Err(self.runtime_bug(MSG));
        };
        let lpc_string = LpcString::Static(index, strings.clone());

        trace!(?lpc_string, "Storing static string");

        let new_ref = lpc_string.into();

        set_location(&mut self.stack, location, new_ref)
    }

    #[instrument(skip_all)]
    #[inline]
    pub(crate) fn handle_store(
        &mut self,
        value_loc: RegisterVariant,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
    ) -> lpc_rs_errors::Result<()> {
        let mut container = get_location(&self.stack, container_loc)?.into_owned();
        let index = &*get_location(&self.stack, index_loc)?;
        let array_idx = if let LpcRef::Int(i) = index { i.0 } else { 0 };

        match container {
            LpcRef::Array(vec_ref) => {
                let mut vec = vec_ref.write();

                let len = vec.len();

                // handle negative indices
                let idx = if array_idx >= 0 {
                    array_idx
                } else {
                    len as LpcIntInner + array_idx
                };

                if idx >= 0 && (idx as usize) < len {
                    vec[idx as usize] = (*get_location(&self.stack, value_loc)?).clone();
                } else {
                    return Err(self.array_index_error(idx, len));
                }

                Ok(())
            }
            LpcRef::Mapping(ref mut map_ref) => {
                let mut map = map_ref.write();

                map.insert(
                    index.clone(),
                    get_location(&self.stack, value_loc)?.into_owned(),
                );

                Ok(())
            }
            x => Err(self.runtime_error(format!("Invalid attempt to take index of `{}`", x))),
        }
    }
}
