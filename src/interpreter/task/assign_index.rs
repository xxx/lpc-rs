use lpc_rs_core::register::RegisterVariant;
use lpc_rs_errors::Result;

use super::{Task, get_location, replace_range::replace_string, set_location};
use crate::interpreter::lpc_ref::LpcRef;

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[inline(never)]
    pub(crate) fn handle_assign_index(
        &mut self,
        value: RegisterVariant,
        target: RegisterVariant,
        index: RegisterVariant,
    ) -> Result<bool> {
        let source = get_location(&self.stack, &self.context.txn, target)?;
        let LpcRef::String(source) = &*source else {
            self.handle_store(value, target, index)?;
            return Ok(false);
        };
        let index = get_location(&self.stack, &self.context.txn, index)?;
        let LpcRef::Int(index) = &*index else {
            return Err(self.runtime_error("String index must be an integer"));
        };
        let len = source.char_count() as i128;
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
        let mut utf8 = [0; 4];
        let replacement = scalar.encode_utf8(&mut utf8);
        let position = position as usize;
        let result = replace_string(source.to_str(), position..position + 1, replacement)
            .ok_or_else(|| {
                self.runtime_error("String element assignment exceeds maximum string length")
            })?;
        set_location(&mut self.stack, &self.context.txn, target, result)?;
        Ok(true)
    }
}
