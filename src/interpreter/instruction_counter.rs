use crate::{errors::LpcError, util::config::Config, Result};
use std::cell::Cell;

/// A struct to encapsulate counting instructions, and erroring in the case of hitting the eval limit
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
            max_instructions: config.max_task_instructions().unwrap_or(0),
        }
    }

    /// Increment the counter by `amount`
    pub fn increment(&self, amount: usize) -> Result<usize> {
        let new_val = self.count.get() + amount;
        self.count.set(new_val);

        if self.max_instructions > 0 && new_val > self.max_instructions {
            return Err(LpcError::new(format!(
                "evaluation limit of `{}` instructions has been reached.",
                self.max_instructions
            )));
        }

        Ok(new_val)
    }
}
