use delegate::delegate;
use crate::interpreter::lpc_constant::LPCConstant;

#[derive(Debug)]
pub struct ConstantPool {
    constants: Vec<LPCConstant>
}

impl ConstantPool {
    pub fn insert(&mut self, constant: LPCConstant) -> usize {
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
            pub fn get(&self, index: usize) -> Option<&LPCConstant>;
        }
    }
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self {
            constants: vec![]
        }
    }
}