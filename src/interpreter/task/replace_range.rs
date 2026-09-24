use lpc_rs_asm::instruction::ReverseRange;
use lpc_rs_core::{LpcIntInner, register::RegisterVariant};
use lpc_rs_errors::Result;
use lpc_rs_utils::{lpc_string::LpcString, string::MAX_STRING_LENGTH};

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
        target: RegisterVariant,
        start: RegisterVariant,
        end: RegisterVariant,
        replacement: RegisterVariant,
    ) -> Result<()> {
        let source = get_location(&self.stack, &self.context.txn, target)?.into_owned();
        let replacement = get_location(&self.stack, &self.context.txn, replacement)?.into_owned();
        let start = get_location(&self.stack, &self.context.txn, start)?.into_owned();
        let end = get_location(&self.stack, &self.context.txn, end)?.into_owned();
        let (LpcRef::Int(start), LpcRef::Int(end)) = (start, end) else {
            return Err(self.runtime_error("Slice assignment bounds must be integers"));
        };
        let result = match (source, replacement) {
            (LpcRef::Array(source), LpcRef::Array(replacement)) => {
                let (source, replacement) = self.context.txn.with(|txn| {
                    let source = txn.read_array(source.id);
                    let replacement = txn.read_array(replacement.id);
                    (source, replacement)
                });
                let (Some(source), Some(replacement)) = (source, replacement) else {
                    return Err(self.runtime_error("Missing array in slice assignment"));
                };
                let range =
                    write_range(start.0, end.0, source.len(), direction).ok_or_else(|| {
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
                LpcRef::Array(self.context.txn.with(|txn| txn.mint_array(result)))
            }
            (LpcRef::String(source), LpcRef::String(replacement)) => {
                self.replace_string_range(&source, start.0, end.0, direction, &replacement)?
            }
            _ => {
                return Err(
                    self.runtime_error("Slice assignment requires two arrays or two strings")
                );
            }
        };
        set_location(&mut self.stack, &self.context.txn, target, result)
    }

    fn replace_string_range(
        &self,
        source: &LpcString,
        start: LpcIntInner,
        end: LpcIntInner,
        direction: Option<ReverseRange>,
        replacement: &LpcString,
    ) -> Result<LpcRef> {
        let text = source.to_str();
        let ascii = text.is_ascii();
        let count = if ascii {
            text.len()
        } else {
            text.chars().count()
        };
        let range = write_range(start, end, count, direction).ok_or_else(|| {
            self.runtime_error(format!(
                "Invalid string slice assignment range: {start}..{end} for length {count}"
            ))
        })?;
        replace_string(text, range, replacement.to_str()).ok_or_else(|| {
            self.runtime_error("String slice assignment exceeds maximum string length")
        })
    }
}

/// Replace a validated scalar interval without allocating intermediate substrings.
pub(super) fn replace_string(
    text: &str,
    range: std::ops::Range<usize>,
    replacement: &str,
) -> Option<LpcRef> {
    let ascii = text.is_ascii();
    let range = if ascii {
        range
    } else {
        let mut chars = text.chars();
        if range.start != 0 {
            chars.nth(range.start - 1);
        }
        let start = text.len() - chars.as_str().len();
        if !range.is_empty() {
            chars.nth(range.len() - 1);
        }
        start..text.len() - chars.as_str().len()
    };
    replace_string_bytes(text, range, replacement)
}

/// Replace a validated UTF-8 byte interval without allocating intermediate substrings.
#[inline]
pub(super) fn replace_string_bytes(
    text: &str,
    range: std::ops::Range<usize>,
    replacement: &str,
) -> Option<LpcRef> {
    let (prefix, suffix) = (&text[..range.start], &text[range.end..]);
    let len = prefix
        .len()
        .checked_add(replacement.len())?
        .checked_add(suffix.len())
        .filter(|len| *len <= MAX_STRING_LENGTH)?;
    let mut result = String::with_capacity(len);
    result.push_str(prefix);
    result.push_str(replacement);
    result.push_str(suffix);
    Some(LpcRef::from(result))
}
