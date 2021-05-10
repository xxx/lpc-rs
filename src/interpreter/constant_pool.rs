use delegate::delegate;

/// Hold constant values, to allow for re-use without copying.
#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct ConstantPool {
    constants: Vec<String>,
}

impl ConstantPool {
    /// An insert that checks for duplicates
    ///
    /// # Arguments
    /// `s` - The [`String`] to insert into the pool.
    pub fn insert(&mut self, s: String) -> usize {
        if let Some(idx) = self.constants.iter().position(|x| *x == s) {
            return idx;
        }

        let idx = self.constants.len();
        self.constants.push(s);
        idx
    }

    delegate! {
        to self.constants {
            #[call(get)]
            pub fn get(&self, index: usize) -> Option<&String>;
        }
    }
}
