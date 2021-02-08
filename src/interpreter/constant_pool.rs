use delegate::delegate;
use crate::interpreter::lpc_value::LPCValue;

/// Hold constant values, to allow for re-use without copying.
#[derive(Debug, Default, Clone)]
pub struct ConstantPool {
    constants: Vec<LPCValue>
}

impl ConstantPool {
    /// An insert that checks for duplicates
    pub fn insert(&mut self, constant: LPCValue) -> usize {
        if let Some(idx) = self.constants.iter().position(|x| *x == constant) {
            return idx;
        }

        let idx = self.constants.len();
        self.constants.push(constant);
        idx
    }

    delegate! {
        to self.constants {
            #[call(get)]
            pub fn get(&self, index: usize) -> Option<&LPCValue>;
        }
    }
}
