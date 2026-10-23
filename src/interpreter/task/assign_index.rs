use std::sync::Arc;

use lpc_rs_core::register::RegisterVariant;
use lpc_rs_errors::Result;
use lpc_rs_utils::{lpc_string::LpcString, string::MAX_STRING_LENGTH};

use super::{Task, get_location, replace_range::replace_string_bytes};
use crate::interpreter::lpc_ref::LpcRef;

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    // Keep string validation and allocation out of the collection store path.
    #[inline(never)]
    pub(super) fn replace_string_index(
        &self,
        source: &Arc<LpcString>,
        value: RegisterVariant,
        index: RegisterVariant,
    ) -> Result<LpcRef> {
        let index = get_location(&self.stack, &self.context.txn, index)?;
        let LpcRef::Int(index) = &*index else {
            return Err(self.runtime_error("String index must be an integer"));
        };
        let text = source.to_str();
        let ascii = text.is_ascii();
        let len = if ascii {
            text.len()
        } else {
            text.chars().count()
        } as i128;
        let position = if index.0 < 0 {
            len + i128::from(index.0)
        } else {
            i128::from(index.0)
        };
        if position < 0 || position >= len {
            return Err(self.runtime_error(format!(
                "Invalid string assignment index: {} for length {len}",
                index.0
            )));
        }
        let value = get_location(&self.stack, &self.context.txn, value)?;
        let LpcRef::Int(value) = &*value else {
            return Err(
                self.runtime_error("String element assignment requires an integer Unicode scalar")
            );
        };
        let scalar = u32::try_from(value.0)
            .ok()
            .and_then(char::from_u32)
            .ok_or_else(|| {
                self.runtime_error("Invalid Unicode scalar in string element assignment")
            })?;
        let position = position as usize;
        let (range, unchanged) = if ascii {
            (
                position..position + 1,
                char::from(text.as_bytes()[position]) == scalar,
            )
        } else {
            let mut chars = text.chars();
            if position != 0 {
                chars.nth(position - 1);
            }
            let start = text.len() - chars.as_str().len();
            let unchanged = chars.next() == Some(scalar);
            (start..text.len() - chars.as_str().len(), unchanged)
        };
        if unchanged && text.len() <= MAX_STRING_LENGTH {
            return Ok(LpcRef::String(Arc::clone(source)));
        }
        let mut utf8 = [0; 4];
        let replacement = scalar.encode_utf8(&mut utf8);
        replace_string_bytes(text, range, replacement).ok_or_else(|| {
            self.runtime_error("String element assignment exceeds maximum string length")
        })
    }
}
