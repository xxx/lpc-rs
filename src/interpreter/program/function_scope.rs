use std::{collections::HashMap, sync::Arc};

use indexmap::IndexMap;
use lpc_rs_core::EFUN;
use ustr::Ustr;

use super::Function;

/// Function names and inherit aliases as seen by one defining program.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct FunctionScope {
    functions: HashMap<String, Ustr>,
    inherits: Box<[Arc<FunctionScope>]>,
    inherit_names: HashMap<String, usize>,
}

impl FunctionScope {
    pub(crate) fn new(
        functions: &IndexMap<Ustr, Function, ahash::RandomState>,
        inherits: Vec<Arc<Self>>,
        inherit_names: HashMap<String, usize>,
    ) -> Self {
        Self {
            functions: functions
                .iter()
                .filter(|(_, function)| !function.is_closure())
                .map(|(&mangled, function)| (function.name().to_string(), mangled))
                .collect(),
            inherits: inherits.into_boxed_slice(),
            inherit_names,
        }
    }

    pub(super) fn inherited_function(&self, namespace: &str, name: &str) -> Option<Ustr> {
        match namespace {
            "" => self
                .inherits
                .iter()
                .rev()
                .find_map(|parent| parent.functions.get(name).copied()),
            EFUN => None,
            alias => self
                .inherit_names
                .get(alias)
                .and_then(|&index| self.inherits.get(index))
                .and_then(|parent| parent.functions.get(name).copied()),
        }
    }
}
