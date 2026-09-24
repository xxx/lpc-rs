use lpc_rs_asm::instruction::ReverseRange;
use lpc_rs_core::{LpcIntInner, register::RegisterVariant};
use lpc_rs_errors::Result;

use super::{Task, get_location, set_location};
use crate::interpreter::{lpc_array::LpcArray, lpc_ref::LpcRef};

fn write_range(
    start: LpcIntInner,
    end: LpcIntInner,
    len: usize,
    direction: Option<ReverseRange>,
) -> Option<std::ops::Range<usize>> {
    let len = len as i128;
    let resolve = |index: LpcIntInner, from_end| {
        let index = i128::from(index);
        if from_end {
            len - index
        } else if index < 0 {
            len + index
        } else {
            index
        }
    };
    let start = resolve(start, direction.is_some_and(ReverseRange::start_from_end));
    let end = resolve(end, direction.is_some_and(ReverseRange::end_from_end)) + 1;
    (0 <= start && start <= end && end <= len).then_some(start as usize..end as usize)
}

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    // Keep allocation and range validation out of the ordinary instruction dispatch path.
    #[inline(never)]
    pub(crate) fn handle_replace_range(
        &mut self,
        direction: Option<ReverseRange>,
        array: RegisterVariant,
        start: RegisterVariant,
        end: RegisterVariant,
        replacement: RegisterVariant,
    ) -> Result<()> {
        let source = get_location(&self.stack, &self.context.txn, array)?.into_owned();
        let replacement = get_location(&self.stack, &self.context.txn, replacement)?.into_owned();
        let (LpcRef::Array(source), LpcRef::Array(replacement)) = (source, replacement) else {
            return Err(self.runtime_error("Slice assignment requires array operands"));
        };
        let start = get_location(&self.stack, &self.context.txn, start)?.into_owned();
        let end = get_location(&self.stack, &self.context.txn, end)?.into_owned();
        let (LpcRef::Int(start), LpcRef::Int(end)) = (start, end) else {
            return Err(self.runtime_error("Slice assignment bounds must be integers"));
        };
        let (source, replacement) = self.context.txn.with(|txn| {
            let source = txn.read_array(source.id);
            let replacement = txn.read_array(replacement.id);
            (source, replacement)
        });
        let (Some(source), Some(replacement)) = (source, replacement) else {
            return Err(self.runtime_error("Missing array in slice assignment"));
        };
        let range = write_range(start.0, end.0, source.len(), direction).ok_or_else(|| {
            self.runtime_error(format!(
                "Invalid array slice assignment range: {}..{} for length {}",
                start.0,
                end.0,
                source.len()
            ))
        })?;
        let len = (source.len() - range.len())
            .checked_add(replacement.len())
            .filter(|len| *len <= isize::MAX as usize / size_of::<LpcRef>())
            .ok_or_else(|| self.runtime_error("Array slice assignment size overflow"))?;
        let mut result = LpcArray::with_capacity(len);
        result.extend(source[..range.start].iter().cloned());
        result.extend(replacement.iter().cloned());
        result.extend(source[range.end..].iter().cloned());
        let result = LpcRef::Array(self.context.txn.with(|txn| txn.mint_array(result)));
        set_location(&mut self.stack, &self.context.txn, array, result)
    }
}
