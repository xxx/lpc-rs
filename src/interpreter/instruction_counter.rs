use std::cell::Cell;

use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;

/// A struct to encapsulate counting instructions, and erroring in the case of
/// hitting the eval limit
#[derive(Debug, Default, Clone)]
pub struct InstructionCounter {
    count: Cell<usize>,
    max_instructions: usize,
}

impl InstructionCounter {
    /// Create a new [`InstructionCounter`], with limits taken from `config`
    pub fn new_from_config(config: &Config) -> Self {
        Self {
            count: Cell::new(0),
            max_instructions: config.max_task_instructions.unwrap_or(0),
        }
    }

    /// Increment the counter by `amount`
    pub fn increment(&self, amount: usize) -> Result<usize> {
        let new_val = self.count.get() + amount;
        self.set(new_val)
    }

    /// Set the counter to `new_val`
    pub fn set(&self, new_val: usize) -> Result<usize> {
        self.count.set(new_val);

        if self.max_instructions > 0 && new_val > self.max_instructions {
            return Err(LpcError::new(format!(
                "evaluation limit of `{}` instructions has been reached.",
                self.max_instructions
            )));
        }

        Ok(new_val)
    }

    /// Get the current instruction count
    pub fn count(&self) -> usize {
        self.count.get()
    }
}
