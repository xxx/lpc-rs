use crate::interpreter::lpc_value::LpcValue;
use delegate::delegate;

/// Hold constant values, to allow for re-use without copying.
#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct ConstantPool {
    constants: Vec<LpcValue>,
}

impl ConstantPool {
    /// An insert that checks for duplicates
    pub fn insert(&mut self, constant: LpcValue) -> usize {
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
            pub fn get(&self, index: usize) -> Option<&LpcValue>;
        }
    }
}
