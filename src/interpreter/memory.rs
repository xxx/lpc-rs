//! On-demand estimates of the current LPC storage reachable from one object.

use std::{collections::HashSet, sync::Arc};

use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::lpc_string::LpcString;
use ustr::Ustr;

use crate::interpreter::{
    function_type::function_ptr::FunctionPtr,
    lpc_array::LpcArray,
    lpc_class::LpcClass,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    process::{Process, ProgramImage},
    stm::{TxnHandle, VarId},
};

const MAX_STEPS: usize = 1_000_000;
const YIELD_EVERY: usize = 512;

pub(crate) fn overflow() -> LpcError {
    LpcError::runtime("object_memory_info: size overflow")
}

#[derive(Default)]
pub(crate) struct Size(usize);

impl Size {
    pub(crate) fn new(bytes: usize) -> Self {
        Self(bytes)
    }

    pub(crate) fn add(&mut self, bytes: usize) -> Result<()> {
        self.0 = self.0.checked_add(bytes).ok_or_else(overflow)?;
        Ok(())
    }

    pub(crate) fn items<T>(&mut self, count: usize) -> Result<()> {
        self.add(count.checked_mul(size_of::<T>()).ok_or_else(overflow)?)
    }

    pub(crate) fn bytes(&self) -> usize {
        self.0
    }
}

pub(crate) struct ObjectMemory {
    pub(crate) object_bytes: usize,
    pub(crate) program_bytes: usize,
    pub(crate) data_bytes: usize,
}

impl ObjectMemory {
    pub(crate) fn fields(&self) -> Result<[(&'static str, i64); 4]> {
        let mut total = Size::new(self.object_bytes);
        total.add(self.program_bytes)?;
        total.add(self.data_bytes)?;
        let values = [
            ("object_bytes", self.object_bytes),
            ("program_bytes", self.program_bytes),
            ("data_bytes", self.data_bytes),
            ("total_bytes", total.bytes()),
        ];
        let mut result = [("", 0); 4];
        for (slot, (key, value)) in result.iter_mut().zip(values) {
            *slot = (key, i64::try_from(value).map_err(|_| overflow())?);
        }
        Ok(result)
    }
}

enum Work {
    Globals(Arc<ProgramImage>, usize),
    Slot(VarId),
    Value(LpcRef),
    Array(Arc<LpcArray>, usize),
    Mapping(Arc<LpcMapping>, usize),
    Class(Arc<LpcClass>, usize),
    Function(Arc<FunctionPtr>, usize, usize),
}

#[derive(Default)]
struct Graph {
    bytes: Size,
    instance_slots: HashSet<VarId>,
    cells: HashSet<VarId>,
    strings: HashSet<usize>,
    interned: HashSet<Ustr>,
    bytes_values: HashSet<usize>,
    byte_slices: HashSet<(usize, usize)>,
    functions: HashSet<usize>,
    pending: Vec<Work>,
}

impl Graph {
    fn step(&mut self, work: Work, txn: &TxnHandle) -> Result<()> {
        match work {
            Work::Globals(image, index) => {
                if index < usize::from(image.program.num_globals) {
                    let slot = image.global(index);
                    self.pending.push(Work::Globals(image, index + 1));
                    self.pending.push(Work::Slot(slot));
                }
            }
            Work::Slot(id) => {
                if self.cells.insert(id) {
                    if !self.instance_slots.contains(&id) {
                        self.bytes.items::<LpcRef>(1)?;
                    }
                    if let Some(value) = txn.with(|t| t.read(id)) {
                        self.pending.push(Work::Value(value));
                    }
                }
            }
            Work::Value(value) => self.value(value, txn)?,
            Work::Array(array, index) => {
                if let Some(value) = array.get(index) {
                    let value = value.clone();
                    self.pending.push(Work::Array(array, index + 1));
                    self.pending.push(Work::Value(value));
                }
            }
            Work::Mapping(mapping, index) => {
                if let Some((key, value)) = mapping.get_index(index) {
                    let (key, value) = (key.clone(), value.clone());
                    self.pending.push(Work::Mapping(mapping, index + 1));
                    self.pending.push(Work::Value(key));
                    self.pending.push(Work::Value(value));
                }
            }
            Work::Class(instance, index) => {
                if let Some(value) = instance.value_at(index) {
                    let value = value.clone();
                    self.pending.push(Work::Class(instance, index + 1));
                    self.pending.push(Work::Value(value));
                }
            }
            Work::Function(function, arg, capture) => {
                if let Some(value) = function.partial_args().get(arg) {
                    let value = value.clone();
                    self.pending
                        .push(Work::Function(function, arg + 1, capture));
                    if let Some(value) = value {
                        self.pending.push(Work::Value(value));
                    }
                } else if let Some(&id) = function.upvalue_ptrs.get(capture) {
                    self.pending
                        .push(Work::Function(function, arg, capture + 1));
                    self.pending.push(Work::Slot(id));
                }
            }
        }
        Ok(())
    }

    fn value(&mut self, value: LpcRef, txn: &TxnHandle) -> Result<()> {
        let missing = || LpcError::runtime("object_memory_info: missing container payload");
        match value {
            LpcRef::String(string) if self.strings.insert(Arc::as_ptr(&string) as usize) => {
                self.bytes.items::<usize>(2)?;
                self.bytes.items::<LpcString>(1)?;
                match &*string {
                    LpcString::Static(text) if self.interned.insert(*text) => {
                        self.bytes.add(text.len())?;
                    }
                    LpcString::Dynamic(text) => self.bytes.add(text.capacity())?,
                    _ => {}
                }
            }
            LpcRef::Bytes(bytes) if self.bytes_values.insert(Arc::as_ptr(&bytes) as usize) => {
                self.bytes.items::<usize>(2)?;
                self.bytes.items::<bytes::Bytes>(1)?;
                if self
                    .byte_slices
                    .insert((bytes.as_ptr() as usize, bytes.len()))
                {
                    self.bytes.add(bytes.len())?;
                }
            }
            LpcRef::Array(cell) if self.cells.insert(cell.id) => {
                let array = txn.with(|t| t.read_array(cell.id)).ok_or_else(missing)?;
                self.bytes.items::<usize>(2)?;
                self.bytes.items::<LpcArray>(1)?;
                if array.array.capacity() != 0 {
                    self.bytes.items::<usize>(2)?;
                    self.bytes.items::<LpcRef>(array.array.capacity())?;
                }
                self.pending.push(Work::Array(array, 0));
            }
            LpcRef::Mapping(cell) if self.cells.insert(cell.id) => {
                let mapping = txn.with(|t| t.read_mapping(cell.id)).ok_or_else(missing)?;
                self.bytes.items::<usize>(2)?;
                self.bytes.items::<LpcMapping>(1)?;
                self.bytes
                    .items::<(LpcRef, LpcRef, usize, usize)>(mapping.mapping.capacity())?;
                self.pending.push(Work::Mapping(mapping, 0));
            }
            LpcRef::Class(cell) if self.cells.insert(cell.id) => {
                let instance = txn.with(|t| t.read_class(cell.id)).ok_or_else(missing)?;
                self.bytes.items::<usize>(2)?;
                self.bytes.add(instance.storage_bytes()?)?;
                self.pending.push(Work::Class(instance, 0));
            }
            LpcRef::Function(function)
                if self.functions.insert(Arc::as_ptr(&function) as usize) =>
            {
                self.bytes.items::<usize>(2)?;
                self.bytes.add(function.storage_bytes()?)?;
                self.pending.push(Work::Function(function, 0, 0));
            }
            _ => {}
        }
        Ok(())
    }
}

pub(crate) async fn object_memory(process: &Process, txn: &TxnHandle) -> Result<ObjectMemory> {
    measure(process, txn, MAX_STEPS).await
}

async fn measure(process: &Process, txn: &TxnHandle, limit: usize) -> Result<ObjectMemory> {
    let image = process.image(txn);
    let object_bytes = process.storage_bytes(&image)?;
    let program_bytes = image.program.storage_bytes().ok_or_else(overflow)?;
    let mut graph = Graph {
        instance_slots: (0..usize::from(image.program.num_globals))
            .map(|index| image.global(index))
            .collect(),
        ..Default::default()
    };
    graph.pending.push(Work::Globals(image, 0));
    let mut steps = 0;
    while let Some(work) = graph.pending.pop() {
        if steps == limit {
            return Err(LpcError::runtime(
                "object_memory_info: traversal limit exceeded",
            ));
        }
        graph.step(work, txn)?;
        steps += 1;
        if steps.is_multiple_of(YIELD_EVERY) {
            tokio::task::yield_now().await;
        }
    }
    Ok(ObjectMemory {
        object_bytes,
        program_bytes,
        data_bytes: graph.bytes.bytes(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::program::Program;

    fn array_object() -> (Process, TxnHandle) {
        let process = Process::new(Program {
            num_globals: 1,
            ..Default::default()
        });
        let txn = TxnHandle::empty();
        txn.with(|t| {
            let data = t.mint_array(LpcArray::from(vec![LpcRef::from(0); 4096].as_slice()));
            t.write(process.initial_image().global(0), LpcRef::Array(data));
        });
        (process, txn)
    }

    #[tokio::test]
    async fn interned_text_is_shared_across_distinct_string_handles() {
        let process = Process::new(Program {
            num_globals: 2,
            ..Default::default()
        });
        let txn = TxnHandle::empty();
        let shared = ustr::ustr(&"s".repeat(4096));
        let different = ustr::ustr(&"t".repeat(4096));
        let string = |text| LpcRef::String(Arc::new(LpcString::Static(text)));
        txn.with(|t| {
            t.write(process.initial_image().global(0), string(shared));
            t.write(process.initial_image().global(1), string(shared));
        });
        let shared_bytes = object_memory(&process, &txn).await.unwrap().data_bytes;
        txn.with(|t| t.write(process.initial_image().global(1), string(different)));
        let separate_bytes = object_memory(&process, &txn).await.unwrap().data_bytes;
        assert_eq!(separate_bytes, shared_bytes + shared.len());
    }

    #[tokio::test]
    async fn large_graphs_fail_instead_of_returning_a_partial_estimate() {
        let (process, txn) = array_object();
        let error = measure(&process, &txn, 16).await.err().unwrap();
        assert_eq!(
            error.to_string(),
            "runtime error: object_memory_info: traversal limit exceeded"
        );
    }

    #[tokio::test(start_paused = true)]
    async fn traversal_yields_so_the_callers_timeout_can_cancel_it() {
        let (process, txn) = array_object();
        assert!(
            tokio::time::timeout(std::time::Duration::ZERO, object_memory(&process, &txn))
                .await
                .is_err()
        );
    }

    #[test]
    fn totals_that_do_not_fit_an_lpc_integer_are_errors() {
        let size = ObjectMemory {
            object_bytes: i64::MAX as usize,
            program_bytes: 1,
            data_bytes: 0,
        };
        assert_eq!(
            size.fields().err().unwrap().to_string(),
            "runtime error: object_memory_info: size overflow"
        );
    }
}
